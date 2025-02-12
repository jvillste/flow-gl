(ns fungl.hierarchical-identity-cache
  (:require [clojure.test :refer [deftest is testing]]
            [medley.core :as medley]
            [fungl.trie :as trie]
            [strict-core :refer :all]
            [fungl.depend :as depend]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.set :as set]))

(defn initial-state []
  {:cache-trie (trie/create-trie)
   :cycle-number 0
   :last-cleanup-cycle-number -1})

(defn create-cache-atom [& [name]]
  (atom (assoc (initial-state)
               :name name)))


(defn add-to-cache! [cache-atom path mapping-id identity-keys value-keys cached-value dependencies]
  (swap! cache-atom
         (fn [cache]
           (update cache
                   :cache-trie
                   trie/update-in-trie
                   path
                   (fn [trie-node]
                     (-> trie-node
                         (assoc :last-accessed (:cycle-number cache)
                                :status :new)
                         (update :mappings
                                 assoc
                                 mapping-id
                                 {:identity-keys identity-keys
                                  :value-keys value-keys
                                  :value cached-value
                                  :dependencies dependencies})))))))

(deftest test-add-to-cache
  (let [cache-atom (create-cache-atom)]
    (add-to-cache! cache-atom
                   [:a]
                   :mapping-1
                   [:identity-key]
                   [:value-key]
                   :value
                   {})
    (is (= {:cache-trie
            {:a {::trie/value {:last-accessed 0,
                               :status :new
                               :mappings
                               {:mapping-1
                                {:identity-keys [:identity-key],
                                 :value-keys [:value-key],
                                 :value :value,
                                 :dependencies {}}}}}},
            :cycle-number 0,
            :last-cleanup-cycle-number -1,
            :name nil}
           @cache-atom))))

(defn- identical-values? [sequence-1 sequence-2]
  (and (= (count sequence-1)
          (count sequence-2))
       (loop [sequence-1 sequence-1
              sequence-2 sequence-2]
         (if (and (empty? sequence-1)
                  (empty? sequence-2))
           true
           (if (identical? (first sequence-1)
                           (first sequence-2))
             (recur (rest sequence-1)
                    (rest sequence-2))
             false)))))

(deftest test-identical-values?
  (is (identical-values? [] [])))

(defn keys-match? [identity-keys value-keys mapping]
  (and (identical-values? identity-keys
                          (:identity-keys mapping))
       (= value-keys
          (:value-keys mapping))))

(defn- get-mapping-from-cache [cache-atom path mapping-id identity-keys value-keys]
  (if-some [trie-node (trie/get-in-trie (:cache-trie @cache-atom)
                                        path)]
    (if-some [mapping (get-in trie-node [:mappings mapping-id])]
      (if (keys-match? identity-keys value-keys mapping)
        mapping
        ::not-found)
      ::not-found)
    ::not-found))

(defn changed-dependency-value? [dependency-value-pair]
  (let [[dependency value] dependency-value-pair]
    (not (if (coll? value)
           (identical? value (depend/current-value dependency))
           (= value (depend/current-value dependency))))))

(defn invalid-mapping? [mapping]
  (some changed-dependency-value?
        (:dependencies mapping)))

(defn cached? [cache-atom path mapping-id identity-keys value-keys]
  (let [mapping (get-mapping-from-cache cache-atom path mapping-id identity-keys value-keys)]
    (not (or (= ::not-found
                mapping)
             (invalid-mapping? mapping)))))

(defn call-with-cache [cache-atom path number-of-identity-arguments function & arguments]
  #_(apply function arguments)
  (let [identity-keys (take number-of-identity-arguments
                            arguments)

        value-keys (drop number-of-identity-arguments
                         arguments)

        mapping (get-mapping-from-cache cache-atom path function identity-keys value-keys)

        invalid? (invalid-mapping? mapping)]

    (if (or invalid?
            (= ::not-found mapping))
      (let [{:keys [result dependencies]} (depend/call-and-return-result-and-dependencies function arguments)]
        (swap! cache-atom update-in [:usage-statistics :miss-count] (fnil inc 0))
        (add-to-cache! cache-atom
                       path
                       function
                       identity-keys
                       value-keys
                       result
                       dependencies)
        result)

      (do (swap! cache-atom
                 (fn [cache]
                   (update cache :cache-trie
                           trie/update-in-trie
                           path
                           (fn [trie-node]
                             (assoc trie-node
                                    :last-accessed (:cycle-number @cache-atom)
                                    :status :reused)))))
          (swap! cache-atom update-in [:usage-statistics :hit-count] (fnil inc 0))
          (:value mapping)))))

(defn remove-unused-mappings [last-cleanup-cycle-number cache-trie]
  (let [status (when-some [last-accessed (-> cache-trie ::trie/value :last-accessed)]
                 (when (< last-cleanup-cycle-number
                          last-accessed)
                   (-> cache-trie ::trie/value :status)))]

    (if (= status :reused)
      cache-trie
      (loop [keys (remove #{::trie/value}
                          (keys cache-trie))
             cache-trie (if (some? status)
                          cache-trie
                          (dissoc cache-trie ::trie/value))]
        (if-let [key (first keys)]
          (recur (rest keys)
                 (medley/remove-vals #{{}}
                                     (update cache-trie
                                             key
                                             (fn [child-branch]
                                               (remove-unused-mappings last-cleanup-cycle-number
                                                                       child-branch)))))
          cache-trie)))))

(deftest test-remove-unused-mappings
  (is (= {}
         (remove-unused-mappings 0
                                 {::trie/value {:status :new
                                                :last-accessed 0}})))

  (is (= {}
         (remove-unused-mappings 0
                                 {:a {::trie/value {:status :new
                                                    :last-accessed 0}}})))


  (is (= {::trie/value {:status :new
                        :last-accessed 1}}
         (remove-unused-mappings 0
                                 {::trie/value {:status :new
                                                :last-accessed 1}})))

  (is (= {::trie/value {:status :new
                        :last-accessed 1}
          :a {::trie/value {:status :reused
                            :last-accessed 1}}}
         (remove-unused-mappings 0
                                 {::trie/value {:status :new
                                                :last-accessed 1}
                                  :a {::trie/value {:status :reused
                                                    :last-accessed 1}}})))

  (is (= {::trie/value {:status :new
                        :last-accessed 1}}
         (remove-unused-mappings 0


                                 {::trie/value {:status :new
                                                :last-accessed 1}
                                  :a {::trie/value {:status :reused
                                                    :last-accessed 0}}})))

  (is (= {:a {::trie/value {:status :reused
                            :last-accessed 1}}}
         (remove-unused-mappings 0
                                 {::trie/value {:status :new
                                                :last-accessed 0}
                                  :a {::trie/value {:status :reused
                                                    :last-accessed 1}}})))

  (is (= {::trie/value {:status :reused
                        :last-accessed 1}
          :a {::trie/value {:status :new
                            :last-accessed 0}}}
         (remove-unused-mappings 0
                                 {::trie/value {:status :reused
                                                :last-accessed 1}
                                  :a {::trie/value {:status :new
                                                    :last-accessed 0}}}))))

(defn remove-unused-mappings! [cache-atom]
  (swap! cache-atom
         (fn [cache]
           (-> cache
               (update :cache-trie
                       (partial remove-unused-mappings
                                (:last-cleanup-cycle-number cache)))
               (assoc :last-cleanup-cycle-number (:cycle-number cache))))))

(defn statistics [cache-atom]
  (merge (:usage-statistics @cache-atom)
         {:mapping-count (trie/value-count (:cache-trie @cache-atom))}))

(def ^:dynamic maximum-number-of-cycles-without-removing-unused-keys 10)

(defn with-cache-cleanup* [cache-atom function]
  (swap! cache-atom dissoc :usage-statistics)
  (let [result (function)]
    (when (< maximum-number-of-cycles-without-removing-unused-keys
             (- (:cycle-number @cache-atom)
                (:last-cleanup-cycle-number @cache-atom)))
      (remove-unused-mappings! cache-atom))
    (swap! cache-atom update :cycle-number inc) ;; TODO: handle cycle-number overflow?
    result))

(defmacro with-cache-cleanup [cache-atom & body]
  `(with-cache-cleanup* ~cache-atom (fn [] ~@body)))

(defn cached-call? [cache-atom path number-of-identity-arguments function & arguments]
  (cached? cache-atom
           path
           function
           (take number-of-identity-arguments arguments)
           (drop number-of-identity-arguments arguments)))

(deftest cache-test
  (binding [maximum-number-of-cycles-without-removing-unused-keys 0]
    (testing "usage"
      (testing "zero arguments"
        (let [call-count (atom 0)
              function (fn []
                         (swap! call-count inc)
                         :result)
              cache-atom (create-cache-atom)]

          (is (= :result (call-with-cache cache-atom [] 0 function)))
          (is (= 1 @call-count))

          (is (= :result (call-with-cache cache-atom [] 0 function)))
          (is (= 1 @call-count))))

      (testing "zero identity arguments"
        (let [call-count (atom 0)
              function (fn [result]
                         (swap! call-count inc)
                         result)
              cache-atom (create-cache-atom)]

          (is (= :result (call-with-cache cache-atom [] 0 function :result)))
          (is (= 1 @call-count))

          (is (= :result (call-with-cache cache-atom [] 0 function :result)))
          (is (= 1 @call-count))))

      (testing "identity and value varguments"
        (let [call-count (atom 0)
              function (fn [identity-key value-key]
                         (swap! call-count inc)
                         [identity-key value-key])
              identity-key #{:a}
              cache-atom (create-cache-atom)]

          (is (= [identity-key :a]
                 (call-with-cache cache-atom [:a] 1 function identity-key :a)))

          (is (= [identity-key :a]
                 (call-with-cache cache-atom [:a] 1 function identity-key :a)))

          (is (= 1 @call-count))

          (is (= [identity-key :a]
                 (call-with-cache cache-atom [:a] 1 function #{:a} :a)))

          (is (= 2 @call-count))

          (is (= [identity-key :b]
                 (call-with-cache cache-atom [:a] 1 function identity-key :b)))

          (is (= 3 @call-count)))))

    (testing "changed dependencies invalidate cache"
      (let [cache-atom (create-cache-atom)
            state-atom (dependable-atom/atom 1)
            function (fn [] @state-atom)]
        (with-cache-cleanup cache-atom
          (is (= 1 (call-with-cache cache-atom [:a] 0 function)))
          (is (cached-call? cache-atom [:a] 0 function))
          (swap! state-atom inc)
          (is (not (cached-call? cache-atom [:a] 0 function)))
          (is (= 2 (call-with-cache cache-atom [:a] 0 function))))))

    (testing "changed dependencies invalidate cache with recursion"
      (let [cache-atom (create-cache-atom)
            state-atom (dependable-atom/atom 1)]
        (letfn [(function [n]
                  (if (= n 0)
                    @state-atom
                    (call-with-cache cache-atom [:a] 0 function (dec n))))]
          (with-cache-cleanup cache-atom
            (is (= 1 (call-with-cache cache-atom [:a] 0 function 1)))
            (is (cached-call? cache-atom [:a] 0 function 1))
            (swap! state-atom inc)
            (is (not (cached-call? cache-atom [:a] 0 function 1)))
            (is (= 2 (call-with-cache cache-atom [:a] 0 function 1)))))))

    (testing "cleanup"

      (testing "old value in same path is removed"
        (let [cache-atom (create-cache-atom)]

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity :a))


          (is (cached-call? cache-atom [:a] 0 identity :a))

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity :b))

          (is (not (cached-call? cache-atom [:a] 0 identity :a)))
          (is (cached-call? cache-atom [:a] 0 identity :b))))

      (testing "unused children are removed when parent changes"
        (let [cache-atom (create-cache-atom)]

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a :b] 0 identity 1)
            (call-with-cache cache-atom [:a] 0 identity 2))

          (is (cached-call? cache-atom [:a :b] 0 identity 1))
          (is (cached-call? cache-atom [:a] 0 identity 2))

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a :b] 0 identity 3)
            (call-with-cache cache-atom [:a] 0 identity 4))


          (is (not (cached-call? cache-atom [:a :b] 0 identity 1)))
          (is (not (cached-call? cache-atom [:a] 0 identity 2)))

          (is (cached-call? cache-atom [:a :b] 0 identity 3))
          (is (cached-call? cache-atom [:a] 0 identity 4))))

      (testing "the same key is found on different paths when a common parent changes
            and results to a change in only one child"
        (let [cache-atom (create-cache-atom)]

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity 1)
            (call-with-cache cache-atom [:a :b] 0 identity 2)
            (call-with-cache cache-atom [:a :c] 0 identity 2))

          (is (cached-call? cache-atom [:a] 0 identity 1))
          (is (cached-call? cache-atom [:a :b] 0 identity 2))
          (is (cached-call? cache-atom [:a :c] 0 identity 2))

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity 4)
            (call-with-cache cache-atom [:a :b] 0 identity 2)
            (call-with-cache cache-atom [:a :c] 0 identity 3))

          (is (not (cached-call? cache-atom [:a] 0 identity 1)))
          (is (not (cached-call? cache-atom [:a :c] 0 identity 2)))

          (is (cached-call? cache-atom [:a] 0 identity 4))
          (is (cached-call? cache-atom [:a :b] 0 identity 2))
          (is (cached-call? cache-atom [:a :c] 0 identity 3))))

      (testing "Having many function calls with the same function for the
            same path during the same cycle is not supported"
        (let [cache-atom (create-cache-atom)]

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity 1)
            (call-with-cache cache-atom [:a] 0 identity 2))

          (is (not (cached-call? cache-atom [:a] 0 identity 1)))
          (is (cached-call? cache-atom [:a] 0 identity 2))

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity 1))

          (is (cached-call? cache-atom [:a] 0 identity 1))
          (is (not (cached-call? cache-atom [:a] 0 identity 2)))))

      (testing "Having many function calls with different functions for the
            same path during the same cycle is supported"
        (let [cache-atom (create-cache-atom)
              function-1 (fn [x] x)
              function-2 (fn [x] x)]

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 function-1 1)
            (call-with-cache cache-atom [:a] 0 function-2 2))

          (is (cached-call? cache-atom [:a] 0 function-1 1))
          (is (cached-call? cache-atom [:a] 0 function-2 2))

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 function-1 1))

          (is (cached-call? cache-atom [:a] 0 function-1 1))
           ;; unused mapping is left to the cache because another mapping was reused in the same path
          (is (cached-call? cache-atom [:a] 0 function-2 2)))))))

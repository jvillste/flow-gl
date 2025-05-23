(ns fungl.hierarchical-identity-cache
  (:require [clojure.test :refer [deftest is testing]]
            [medley.core :as medley]
            [fungl.trie :as trie]
            ;; [strict-core :refer :all]
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
  (swap! cache-atom update-in [:usage-statistics :add-to-cache mapping-id] (fnil inc 0))
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

(defn remove-dependencies! [cache-atom path mapping-id]
  (swap! cache-atom
         (fn [cache]
           (update cache
                   :cache-trie
                   trie/update-in-trie
                   path
                   (fn [trie-node]
                     (-> trie-node
                         (update :mappings
                                 update
                                 mapping-id
                                 assoc :dependencies {})))))))

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

(defn- get-mapping-from-cache-without-keys [cache path mapping-id]
  (when-some [trie-node (trie/get-in-trie (:cache-trie cache)
                                          path)]
    (when-some [mapping (get-in trie-node [:mappings mapping-id])]
      mapping)))

(defn- get-mapping-from-cache [cache path mapping-id identity-keys value-keys]
  (if-some [mapping (get-mapping-from-cache-without-keys cache path mapping-id)]
    (if (keys-match? identity-keys value-keys mapping)
      mapping
      ::not-found)
    ::not-found))

(defn changed-dependency-value? [dependency-value-pair]
  (let [[dependency value] dependency-value-pair]
    ;; (when (not (if (coll? value)
    ;;              (identical? value (depend/current-value dependency))
    ;;              (= value (depend/current-value dependency))))
    ;;   (def the-value value)
    ;;   (def the-current-value (depend/current-value dependency))
    ;;   (println "changed dependency" (:name dependency) "equal?" (= value (depend/current-value dependency))))
    (not (if (coll? value)
           (identical? value (depend/current-value dependency))
           (= value (depend/current-value dependency))))))

(defn invalid-mapping? [mapping]
  (some changed-dependency-value?
        (:dependencies mapping)))

(defn- cached? [cache-atom path mapping-id identity-keys value-keys]
  (let [mapping (get-mapping-from-cache @cache-atom path mapping-id identity-keys value-keys)]
    (not (or (= ::not-found
                mapping)
             (invalid-mapping? mapping)))))

(defn select-identity-keys [number-of-identity-arguments arguments]
  (if (> 0 number-of-identity-arguments)
    (take-last (abs number-of-identity-arguments)
               arguments)
    (take number-of-identity-arguments
          arguments)))

(defn select-value-keys [number-of-identity-arguments arguments]
  (if (> 0 number-of-identity-arguments)
    (drop-last (abs number-of-identity-arguments)
               arguments)
    (drop number-of-identity-arguments
          arguments)))

(defn get-value-with-path-only [cache-atom path mapping-id]
  (if-some [mapping (get-mapping-from-cache-without-keys @cache-atom
                                                         path
                                                         mapping-id)]
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
        (:value mapping))
    (do (swap! cache-atom update-in [:usage-statistics :get-value-with-path-only-misses mapping-id] (fnil inc 0))
        ::not-found)))

(defn call-with-path-only-cache [cache-atom path function & arguments]
  (if-some [mapping (get-mapping-from-cache-without-keys @cache-atom
                                                         path
                                                         function)]
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
        (:value mapping))

    (let [result (apply function arguments)]
      (swap! cache-atom update-in [:usage-statistics :miss-count] (fnil inc 0))
      (add-to-cache! cache-atom
                     path
                     function
                     []
                     []
                     result
                     [])
      result)))

(defn call-with-cache [cache-atom path number-of-identity-arguments function & arguments]
  #_(apply function arguments)
  (let [identity-keys (select-identity-keys number-of-identity-arguments arguments)

        value-keys (select-value-keys number-of-identity-arguments arguments)

        mapping (get-mapping-from-cache @cache-atom path function identity-keys value-keys)

        invalid? (invalid-mapping? mapping)]

    (when invalid?
      (swap! cache-atom
             update-in
             [:usage-statistics
              :invalid-mappings
              [function (->> (:dependencies mapping)
                             (filter changed-dependency-value?)
                             (map (comp :name first))
                             (sort))]]
             (fnil inc 0)))

    (when (= ::not-found mapping)
      (swap! cache-atom
             update-in
             [:usage-statistics
              :not-found-mappings
              function]
             (fnil inc 0)))

    #_(when true
        #_(or invalid?
              (= ::not-found mapping))
        #_(= path [:view-call :view-call 0 0 :buttons :meta-node :view-call])
        (println "call-with-cache"
                 function
                 path
                 (if (= ::not-found
                        mapping)
                   "not found"
                   "found")
                 (if invalid?
                   "invalid"
                   "valid")
                 (for [dependency-value-pair (:dependencies mapping)]
                   [(:name (first dependency-value-pair))
                    (if (changed-dependency-value? dependency-value-pair)
                      "invalid"
                      "unchanged")])))


    (if (or invalid?
            (= ::not-found mapping))
      (do ;; (println "cache miss for " function "invalid?" invalid? "not found?" (= ::not-found mapping))
        (let [{:keys [result dependencies]} (depend/call-and-return-result-and-dependencies function arguments)]

          (swap! cache-atom update-in [:usage-statistics :miss-count] (fnil inc 0))
          (add-to-cache! cache-atom
                         path
                         function
                         identity-keys
                         value-keys
                         result
                         dependencies)
          result))

      (do ;; (println "cache hit for " function)
        (depend/add-dependencies (:dependencies mapping))
        (swap! cache-atom
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
  (let [status (or (when-some [last-accessed (-> cache-trie ::trie/value :last-accessed)]
                     (when (< last-cleanup-cycle-number
                              last-accessed)
                       (-> cache-trie ::trie/value :status)))
                   :unused)]
    (if (= status :reused)
      cache-trie
      (loop [keys (remove #{::trie/value}
                          (keys cache-trie))
             cache-trie (if (= :unused status)
                          (dissoc cache-trie ::trie/value)
                          cache-trie)]
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

(defn print-statistics [cache-atom]
  (let [statistics (statistics cache-atom)
        frequency-keys [:add-to-cache
                        :get-value-with-path-only-misses
                        :invalid-mappings
                        :not-found-mappings]]
    (println (:name @cache-atom))
    (doseq [frequency-key frequency-keys]
      (prn frequency-key)
      (doseq [[mapping-id frequency] (->> statistics
                                          frequency-key
                                          (sort-by second)
                                          reverse)]
        (print "    ")
        (prn mapping-id frequency)))
   (prn (apply dissoc statistics frequency-keys))))

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
           (select-identity-keys number-of-identity-arguments arguments)
           (select-value-keys number-of-identity-arguments arguments)))

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

          (is (= 3 @call-count))

          (testing "identity key as the last argument"
            (is (= [:b identity-key]
                   (call-with-cache cache-atom [:b] -1 function :b identity-key)))

            (is (= 4 @call-count))

            (is (= [:b identity-key]
                   (call-with-cache cache-atom [:b] -1 function :b identity-key)))

            (is (= 4 @call-count))

            (is (= [:b identity-key]
                   (call-with-cache cache-atom [:b] -1 function :b #{:a})))

            (is (= 5 @call-count))))))

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

    (testing "two separate dependables invalidate cache"
      (let [cache-atom (create-cache-atom)
            state-1-atom (dependable-atom/atom 1)
            function-1 (fn [] @state-1-atom)
            state-2-atom (dependable-atom/atom 1)
            function-2 (fn [] @state-2-atom)
            root-function (fn []
                            [(call-with-cache cache-atom [1] 0 function-1)
                             (call-with-cache cache-atom [2] 0 function-2)])]

        (with-cache-cleanup cache-atom
          (is (= [1 1]
                 (call-with-cache cache-atom [] 0 root-function))))

        (swap! state-1-atom inc)

        (with-cache-cleanup cache-atom
          (is (= [2 1]
                 (call-with-cache cache-atom [] 0 root-function))))

        (swap! state-2-atom inc)

        (with-cache-cleanup cache-atom
          (is (= [2 2]
                 (call-with-cache cache-atom [] 0 root-function))))))

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

(defn dependency-nodes [cache]
  (->> (trie/values (:cache-trie cache))
       (mapcat (fn [[path value]]
                 (for [[function mapping] (:mappings value)]
                   {:path path
                    :function function
                    :dependencies (:dependencies mapping)})))))

(deftest test-dependenciy-nodes
  (let [cache-atom (create-cache-atom)
        state-atom (dependable-atom/atom "state" 1)
        function (fn [] @state-atom)]
    (call-with-cache cache-atom [:a] 0 function)
    (is (= [{:path [:a],
             :function function
             :dependencies {state-atom 1}}]
           (dependency-nodes @cache-atom)))))

(defn filter-invalid-dependencies [dependency-nodes]
  (->> dependency-nodes
       (map (fn [node]
              (update node :dependencies
                      (partial medley/filter-kv
                               (comp changed-dependency-value?
                                     vector)))))
       (remove (fn [node]
                 (empty? (:dependencies node))))))

(deftest test-filter-invalid-dependencies
  (let [cache-atom (create-cache-atom)
        state-atom (dependable-atom/atom "state" 1)
        function (fn [] @state-atom)]
    (call-with-cache cache-atom [:a] 0 function)
    (is (= [] (filter-invalid-dependencies (dependency-nodes @cache-atom))))
    (swap! state-atom inc)
    (is (= [{:path [:a],
             :function function
             :dependencies {state-atom 1}}]
           (filter-invalid-dependencies (dependency-nodes @cache-atom))))))

(defn print-dependency-nodes [dependency-nodes]
  (run! (fn [{:keys [path function dependencies]}]
          (println function path (map :name (keys dependencies))))
        dependency-nodes))

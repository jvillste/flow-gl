(ns fungl.hierarchical-identity-cache
  (:require [clojure.test :refer [deftest is testing]]
            [medley.core :as medley]
            [fungl.trie :as trie]
            [strict-core :refer :all]))

(defn initial-state []
  {:cache-trie (trie/create-trie)
   :cycle-number 0
   :last-cleanup-cycle-number -1})

(defn create-cache-atom []
  (atom (initial-state)))

(defn add-to-cache! [cache-atom path identity-keys value-keys cached-value]
  (swap! cache-atom
         (fn [cache]
           (update cache
                   :cache-trie
                   trie/assoc-trie
                   path
                   {:identity-keys identity-keys
                    :value-keys value-keys
                    :value cached-value
                    :status :new
                    :last-accessed (:cycle-number cache)}))))

(deftest test-add-to-cache
  (let [cache-atom (create-cache-atom)]
    (add-to-cache! cache-atom
                   [:a]
                   [:identity-key]
                   [:value-key]
                   :value)
    (is (= {:cache-trie
            {:a {::trie/value {:identity-keys [:identity-key],
                               :value-keys [:value-key],
                               :value :value,
                               :status :new,
                               :last-accessed 0}}},
            :cycle-number 0,
            :last-cleanup-cycle-number -1}
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

(defn- get-value-from-cache [cache-atom path identity-keys value-keys]
  (if-some [mapping (trie/get-in-trie (:cache-trie @cache-atom)
                                      path)]
    (if (and (identical-values? identity-keys
                                (:identity-keys mapping))
             (= value-keys
                (:value-keys mapping)))
      (:value mapping)
      ::not-found)
    ::not-found))

(deftest test-get-value-from-cache
  (is (= :value
         (let [identity-key {:a :b}
               cache-atom (create-cache-atom)]
           (add-to-cache! cache-atom
                          []
                          [identity-key]
                          [{:c :d}]
                          :value)
           (get-value-from-cache cache-atom
                                 []
                                 [identity-key]
                                 [{:c :d}]))))

  (testing "value can be nil"
    (is (= nil
           (let [identity-key {:a :b}
                 cache-atom (create-cache-atom)]
             (add-to-cache! cache-atom
                            []
                            [identity-key]
                            [{:c :d}]
                            nil)
             (get-value-from-cache cache-atom
                                   []
                                   [identity-key]
                                   [{:c :d}])))))

  (testing "equality is not enough for identity keys"
    (is (= ::not-found
           (let [cache-atom (create-cache-atom)]
             (add-to-cache! cache-atom
                            []
                            [{:a :b}]
                            [{:c :d}]
                            nil)
             (get-value-from-cache cache-atom
                                   []
                                   [{:a :b}]
                                   [{:c :d}]))))))

(defn cached? [cache-atom path identity-keys value-keys]
  (not (= ::not-found
          (get-value-from-cache cache-atom path identity-keys value-keys))))

(defn call-with-cache [cache-atom path number-of-identity-arguments function & arguments]
  #_(apply function arguments)
  (let [identity-keys (concat [function]
                              (take number-of-identity-arguments
                                    arguments))

        value-keys (drop number-of-identity-arguments
                         arguments)

        value (get-value-from-cache cache-atom path identity-keys value-keys)]

    (if (= ::not-found value)
      (let [value (apply function arguments)]
        (swap! cache-atom update-in [:usage-statistics :miss-count] (fnil inc 0))
        (add-to-cache! cache-atom
                       path
                       identity-keys
                       value-keys
                       value)
        value)
      (do (swap! cache-atom
                 update
                 :cache-trie
                 trie/update-in-trie
                 path
                 (fn [mapping]
                   (assoc mapping :status :reused
                          :last-accessed (:cycle-number @cache-atom))))
          (swap! cache-atom update-in [:usage-statistics :hit-count] (fnil inc 0))
          value))))

(defn remove-unused-mappings
  ([last-cleanup-cycle-number cache-trie]
   (remove-unused-mappings :root
                           last-cleanup-cycle-number
                           cache-trie))

  ([closest-parent-status last-cleanup-cycle-number cache-trie]
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
                                                (remove-unused-mappings (or status
                                                                            closest-parent-status)
                                                                        last-cleanup-cycle-number
                                                                        child-branch)))))
           cache-trie))))))

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
         {:mapping-count (count (apply concat (vals (:hash-to-mappings @cache-atom))))}))

(def ^:dynamic maximum-number-of-cycles-without-removing-unused-keys 100)

(defn with-cache-cleanup* [cache-atom function]
  (swap! cache-atom dissoc :usage-statistics)
  (let [result (function)]
    (when (< maximum-number-of-cycles-without-removing-unused-keys
             (- (:cycle-number @cache-atom)
                (:last-cleanup-cycle-number @cache-atom)))
      (remove-unused-mappings! cache-atom))
    (swap! cache-atom update :cycle-number inc)
    result))

(defmacro with-cache-cleanup [cache-atom & body]
  `(with-cache-cleanup* ~cache-atom (fn [] ~@body)))

(defn cached-call? [cache-atom path number-of-identity-arguments function & arguments]
  (cached? cache-atom
           path
           (into [function]
                 (take number-of-identity-arguments arguments))
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


      (testing "some values are reused and some are not in the same path.
            Having many function calls for the same path during the same cycle
            is not supported by the cache now."
        (let [cache-atom (create-cache-atom)]

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity 1)
            (call-with-cache cache-atom [:a] 0 identity 2))

          (is (cached-call? cache-atom [:a] 0 identity 1))
          (is (cached-call? cache-atom [:a] 0 identity 2))

          (with-cache-cleanup cache-atom
            (call-with-cache cache-atom [:a] 0 identity 1))

          (is (cached-call? cache-atom [:a] 0 identity 1))
          (is (cached-call? cache-atom [:a] 0 identity 2)) ;; unused mapping is left to the cache
          )))))

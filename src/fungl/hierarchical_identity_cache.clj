(ns fungl.hierarchical-identity-cache
  (:require [clojure.test :refer [deftest is testing]]
            [medley.core :as medley]
            [fungl.trie :as trie]
            [strict-core :refer :all]))

(defn initial-state []
  {:hash-to-used-keys {}
   :hash-to-mappings {}
   :used-paths-trie (trie/create-trie)})

(defn create-cache-atom []
  (atom (initial-state)))

(defn add-to-cache! [cache-atom path identity-keys value-keys value]
  (swap! cache-atom
         update-in
         [:hash-to-mappings (hash [identity-keys value-keys])]
         (fnil conj [])
         [path identity-keys value-keys value]))

(deftest test-add-to-cache
  (let [cache-atom (create-cache-atom)]
    (add-to-cache! cache-atom
                   []
                   [:identity-key]
                   [:value-key]
                   :value)
    (is (= {:hash-to-used-keys {},
            :used-paths-trie {}
            :hash-to-mappings {83065300 [[[] [:identity-key] [:value-key] :value]]}}
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

(defn- add-used-key! [cache-atom path usage identity-keys value-keys]
  (swap! cache-atom
         update
         :used-paths-trie
         trie/add-to-trie
         path
         usage)
  (swap! cache-atom
         update-in
         [:hash-to-used-keys
          (hash [identity-keys value-keys])]
         (fn [used-keys]
           (if (nil? used-keys)
             [[identity-keys value-keys]]
             (if (loop [mappings used-keys]
                   (if-some [[cached-identity-keys cached-value-keys _value] (first mappings)]
                     (if (and (identical-values? identity-keys
                                                 cached-identity-keys)
                              (= value-keys
                                 cached-value-keys))
                       true
                       (recur (rest mappings)))
                     false))
               used-keys
               (conj used-keys [identity-keys value-keys]))))))

(deftest test-add-used-key!
  (let [cache-atom (create-cache-atom)]
    (add-used-key! cache-atom
                   []
                   :new
                   [:identity-key]
                   [:value-key])
    (is (= {:hash-to-used-keys {83065300 [[[:identity-key] [:value-key]]]},
            :hash-to-mappings {}
            :used-paths-trie {::trie/value :new}}
           @cache-atom))

    (add-used-key! cache-atom
                   []
                   :reused
                   [:identity-key]
                   [:value-key])

    (is (= {:hash-to-used-keys {83065300 [[[:identity-key] [:value-key]]]},
            :hash-to-mappings {}
            :used-paths-trie {::trie/value :reused}}
           @cache-atom))))

(defn- get-value-from-cache [cache-atom identity-keys value-keys]
  (loop [mappings (get-in @cache-atom
                          [:hash-to-mappings (hash [identity-keys value-keys])])]
    (if-some [[_path cached-identity-keys cached-value-keys value] (first mappings)]
      (if (and (identical-values? identity-keys
                                  cached-identity-keys)
               (= value-keys
                  cached-value-keys))
        value
        (recur (rest mappings)))
      ::not-found)))

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
                                   [{:a :b}]
                                   [{:c :d}]))))))


;; (defn get-from-cache [cache-atom path identity-keys value-keys]
;;   (add-used-key! cache-atom path identity-keys value-keys)
;;   (get-value-from-cache cache-atom identity-keys value-keys))

(defn cached? [cache-atom identity-keys value-keys]
  (not (= ::not-found
          (get-value-from-cache cache-atom identity-keys value-keys))))

(defn reset-usage-tracking! [cache-atom]
  (swap! cache-atom
         (fn [cache]
           (-> cache
               (assoc :hash-to-used-keys {})
               (assoc :used-paths-trie (trie/create-trie))))))

(defn- used-key? [cache identity-keys value-keys]
  (if-some [used-keys (get-in cache
                              [:hash-to-used-keys (hash [identity-keys value-keys])])]
    (loop [used-keys used-keys]
      (if-some [[used-identity-keys used-value-keys] (first used-keys)]
        (if (and (identical-values? identity-keys
                                    used-identity-keys)
                 (= value-keys
                    used-value-keys))
          true
          (recur (rest used-keys)))
        false))
    false))

(deftest test-used-key?
  (let [cache-atom (create-cache-atom)]
    (add-used-key! cache-atom [] :reused [] [])
    (is (used-key? @cache-atom [] []))
    (is (not (used-key? @cache-atom [1] [2])))))

(defn remove-unused-keys! [cache-atom]
  (swap! cache-atom
         (fn [cache]
           (update cache
                   :hash-to-mappings
                   (fn [hash-to-mappings]
                     (->> hash-to-mappings
                          (medley/map-vals (fn [cached-mappings]
                                             (filter (fn [[path cached-identity-keys cached-value-keys]]
                                                       (case (trie/last-value-on-path (:used-paths-trie cache)
                                                                                      path)
                                                         ;; TOOD: If last-value-on-path would tell if the value
                                                         ;; was on the same path or on a prefix, we would only need
                                                         ;; to call used-key? when the value was on the same path.
                                                         ;; Calculating the hash of the keys in used-key? was a performance
                                                         ;; bottleneck when it was done on every call on this function.
                                                         :new (used-key? @cache-atom
                                                                         cached-identity-keys
                                                                         cached-value-keys)

                                                         :reused true

                                                         false))
                                                     cached-mappings)))
                          (medley/remove-vals empty?)))))))

(defn statistics [cache-atom]
  (merge (:usage-statistics @cache-atom)
         {:mapping-count (count (apply concat (vals (:hash-to-mappings @cache-atom))))}))

(defn with-cache-cleanup* [cache-atom function]
  (reset-usage-tracking! cache-atom)
  (swap! cache-atom dissoc :usage-statistics)
  (let [result (function)]
    (remove-unused-keys! cache-atom)
    result))

(defmacro with-cache-cleanup [cache-atom & body]
  `(with-cache-cleanup* ~cache-atom (fn [] ~@body)))

;; TODO: when root node is cached everything else is removed from the cache and when something changes everything needs to be recalculated
;; TODO: allow passing anchestor? that tells if a cache key is anchestor of another cache key and use that to
;; keep children in cache when some anchestor is used
(defn call-with-cache [cache-atom path number-of-identity-arguments function & arguments]
  #_(apply function arguments)
  (let [identity-keys (concat [function]
                              (take number-of-identity-arguments
                                    arguments))

        value-keys (drop number-of-identity-arguments
                         arguments)

        value (get-value-from-cache cache-atom identity-keys value-keys)]

    (if (= ::not-found value)
      (let [value (apply function arguments)]
        (swap! cache-atom update-in [:usage-statistics :miss-count] (fnil inc 0))
        (add-used-key! cache-atom path :new identity-keys value-keys)
        (add-to-cache! cache-atom
                       path
                       identity-keys
                       value-keys
                       value)
        value)
      (do (add-used-key! cache-atom path :reused identity-keys value-keys)
          (swap! cache-atom update-in [:usage-statistics :hit-count] (fnil inc 0))
          value))))

(defn cached-call? [cache-atom number-of-identity-arguments function & arguments]
  (cached? cache-atom
           (into [function]
                 (take number-of-identity-arguments arguments))
           (drop number-of-identity-arguments arguments)))

(deftest cache-test
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


        (is (cached-call? cache-atom 0 identity :a))

        (with-cache-cleanup cache-atom
          (call-with-cache cache-atom [:a] 0 identity :b))

        (is (not (cached-call? cache-atom 0 identity :a)))
        (is (cached-call? cache-atom 0 identity :b))))

    (testing "unused children are removed when parent changes"
      (let [cache-atom (create-cache-atom)]

        (with-cache-cleanup cache-atom
          (call-with-cache cache-atom [:a :b] 0 identity 1)
          (call-with-cache cache-atom [:a] 0 identity 2))

        (is (cached-call? cache-atom 0 identity 1))
        (is (cached-call? cache-atom 0 identity 2))

        (with-cache-cleanup cache-atom
          (call-with-cache cache-atom [:a :b] 0 identity 3)
          (call-with-cache cache-atom [:a] 0 identity 4))


        (is (not (cached-call? cache-atom 0 identity 1)))
        (is (not (cached-call? cache-atom 0 identity 2)))

        (is (cached-call? cache-atom 0 identity 3))
        (is (cached-call? cache-atom 0 identity 4))))

    (testing "the same key is found on different paths when a common parent changes
            and results to a change in only one child"
      (let [cache-atom (create-cache-atom)]

        (with-cache-cleanup cache-atom
          (call-with-cache cache-atom [:a] 0 identity 1)
          (call-with-cache cache-atom [:a :b] 0 identity 2)
          (call-with-cache cache-atom [:a :c] 0 identity 2))

        (is (cached-call? cache-atom 0 identity 1))
        (is (cached-call? cache-atom 0 identity 2))

        (with-cache-cleanup cache-atom
          (call-with-cache cache-atom [:a] 0 identity 4)
          (call-with-cache cache-atom [:a :b] 0 identity 2)
          (call-with-cache cache-atom [:a :b] 0 identity 3))

        (is (not (cached-call? cache-atom 0 identity 1)))

        (is (cached-call? cache-atom 0 identity 2))
        (is (cached-call? cache-atom 0 identity 3))
        (is (cached-call? cache-atom 0 identity 4))))


    (testing "some values are reused and some are not in the same path.
            Having many function calls for the same path during the same cycle
            is not supported by the cache now."
      (let [cache-atom (create-cache-atom)]

        (with-cache-cleanup cache-atom
          (call-with-cache cache-atom [:a] 0 identity 1)
          (call-with-cache cache-atom [:a] 0 identity 2))

        (is (cached-call? cache-atom 0 identity 1))
        (is (cached-call? cache-atom 0 identity 2))

        (with-cache-cleanup cache-atom
          (call-with-cache cache-atom [:a] 0 identity 1))

        (is (cached-call? cache-atom 0 identity 1))
        (is (cached-call? cache-atom 0 identity 2)) ;; unused mapping is left to the cache
        ))))

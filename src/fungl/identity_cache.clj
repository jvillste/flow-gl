(ns fungl.identity-cache
  (:require [clojure.test :refer [deftest is testing]]
            [medley.core :as medley]))

(defn create-cache-atom []
  (atom {:hash-to-used-keys {}
         :hash-to-mappings {}}))

(defn add-to-cache! [cache-atom identity-keys value-keys value]
  (swap! cache-atom
         update-in
         [:hash-to-mappings (hash [identity-keys value-keys])]
         (fnil conj [])
         [identity-keys value-keys value]))

(deftest test-add-to-cache
  (let [cache-atom (create-cache-atom)]
    (add-to-cache! cache-atom
                   [:identity-key]
                   [:value-key]
                   :value)
    (is (= {:hash-to-used-keys {},
            :hash-to-mappings {83065300 [[[:identity-key] [:value-key] :value]]}}
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

(defn- add-used-key! [cache-atom identity-keys value-keys]
  (swap! cache-atom
         update-in
         [:hash-to-used-keys
          (hash [identity-keys value-keys])]
         (fn [used-keys]
           (if (nil? used-keys)
             [[identity-keys value-keys]]
             (if (loop [mappings used-keys]
                   (if-some [[cached-identity-keys cached-value-keys value] (first mappings)]
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
                   [:identity-key] [:value-key])
    (is (= {:hash-to-used-keys {83065300 [[[:identity-key] [:value-key]]]},
            :hash-to-mappings {}}
           @cache-atom))

    (add-used-key! cache-atom
                   [:identity-key] [:value-key])

    (is (= {:hash-to-used-keys {83065300 [[[:identity-key] [:value-key]]]},
            :hash-to-mappings {}}
           @cache-atom))))

(defn- get-value-from-cache [cache-atom identity-keys value-keys]
  (loop [mappings (get-in @cache-atom
                          [:hash-to-mappings (hash [identity-keys value-keys])])]
    (if-some [[cached-identity-keys cached-value-keys value] (first mappings)]
      (if (and (identical-values? identity-keys
                                  cached-identity-keys)
               (= value-keys
                  cached-value-keys))
        value
        (recur (rest mappings)))
      ::not-found)))

(defn get-from-cache [cache-atom identity-keys value-keys]
  (add-used-key! cache-atom identity-keys value-keys)
  (get-value-from-cache cache-atom identity-keys value-keys))

(deftest test-get-from-cache
  (is (= :value
         (let [identity-key {:a :b}
               cache-atom (create-cache-atom)]
           (add-to-cache! cache-atom
                          [identity-key]
                          [{:c :d}]
                          :value)
           (get-from-cache cache-atom
                           [identity-key]
                           [{:c :d}]))))

  (testing "value can be nil"
    (is (= nil
           (let [identity-key {:a :b}
                 cache-atom (create-cache-atom)]
             (add-to-cache! cache-atom
                            [identity-key]
                            [{:c :d}]
                            nil)
             (get-from-cache cache-atom
                             [identity-key]
                             [{:c :d}])))))

  (testing "equality is not enough for identity keys"
    (is (= ::not-found
           (let [cache-atom (create-cache-atom)]
             (add-to-cache! cache-atom
                            [{:a :b}]
                            [{:c :d}]
                            nil)
             (get-from-cache cache-atom
                             [{:a :b}]
                             [{:c :d}]))))))

(defn cached? [cache-atom identity-keys value-keys]
  (not (= ::not-found
          (get-value-from-cache cache-atom identity-keys value-keys))))

(defn reset-usage-tracking! [cache-atom]
  (swap! cache-atom assoc :hash-to-used-keys {}))

(defn- used-key? [cache-atom parent-key? identity-keys value-keys]
  (if-some [used-keys (get-in @cache-atom
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
    (add-used-key! cache-atom [] [])
    (is (used-key? cache-atom [] []))
    (is (not (used-key? cache-atom [1] [2])))))

(defn remove-unused-keys! [cache-atom]
  (swap! cache-atom
         (fn [cache]
           (update cache
                   :hash-to-mappings
                   (fn [hash-to-mappings]
                     (->> hash-to-mappings
                          (medley/map-vals (fn [cached-mappings]
                                             (filter (fn [[cached-identity-keys cached-value-keys]]
                                                       (used-key? cache-atom
                                                                  cached-identity-keys
                                                                  cached-value-keys))
                                                     cached-mappings)))
                          (medley/remove-vals empty?)))))))

;; TODO: allow passing anchestor? that tells if a cache key is anchestor of another cache key and use that to
;; keep children in cache when some anchestor is used
(defn call-with-cache [cache-atom number-of-identity-arguments function & arguments]
  (let [identity-keys (concat [function]
                              (take number-of-identity-arguments
                                    arguments))

        value-keys (drop number-of-identity-arguments
                         arguments)

        value (get-from-cache cache-atom
                              identity-keys
                              value-keys)]

    (if (= ::not-found value)
      (let [value (apply function arguments)]
        (add-to-cache! cache-atom
                       identity-keys
                       value-keys
                       value)
        value)
      value)))

(defn cached-call? [cache-atom number-of-identity-arguments function & arguments]
  (cached? cache-atom
           (into [function]
                 (take number-of-identity-arguments arguments))
           (drop number-of-identity-arguments arguments)))

(deftest cache-test
  (testing "usage tracking with both identity and value arguments"
    (let [call-count (atom 0)
          function (fn [identity-key value-key]
                     (swap! call-count inc)
                     [identity-key value-key])
          cache-atom (create-cache-atom)
          identity-key {:a :b}]

      (call-with-cache cache-atom 1 function identity-key :a)
      (is (= 1 @call-count))

      (call-with-cache cache-atom 1 function identity-key :a)
      (is (= 1 @call-count))

      (call-with-cache cache-atom 1 function identity-key :b)
      (is (= 2 @call-count))

      (is (= [[[[function {:a :b}]
                [:a]]]

              [[[function {:a :b}]
                [:b]]]]

             (-> @cache-atom
                 :hash-to-used-keys
                 vals)))

      (reset-usage-tracking! cache-atom)

      (is (= {}
             (-> @cache-atom
                 :hash-to-used-keys)))

      (call-with-cache cache-atom 1 function identity-key :b)
      (is (= 2 @call-count))

      (is (= [[[[function {:a :b}]
                [:b]]]]
             (-> @cache-atom
                 :hash-to-used-keys
                 vals)))

      (is (= [[[[function {:a :b}]
                [:a]
                [{:a :b} :a]]]
              [[[function {:a :b}]
                [:b]
                [{:a :b} :b]]]]
             (-> @cache-atom
                 :hash-to-mappings
                 vals)))

      (is (cached-call? cache-atom 1 function identity-key :a))
      (is (cached-call? cache-atom 1 function identity-key :b))

      (is (= [[[[function {:a :b}]
                [:b]]]]
             (-> @cache-atom
                 :hash-to-used-keys
                 vals)))

      (remove-unused-keys! cache-atom)

      (is (not (cached-call? cache-atom 1 function identity-key :a)))
      (is (cached-call? cache-atom 1 function identity-key :b))

      (call-with-cache cache-atom 1 function identity-key :b)
      (is (= 2 @call-count))

      (call-with-cache cache-atom 1 function identity-key :a)
      (is (= 3 @call-count))))

  (testing "zero arguments"
    (let [call-count (atom 0)
          function (fn []
                     (swap! call-count inc)
                     :result)
          cache-atom (create-cache-atom)]

      (call-with-cache cache-atom 0 function)
      (is (= 1 @call-count))

      (call-with-cache cache-atom 0 function)
      (is (= 1 @call-count))))

  (testing "zero identity arguments"
    (let [call-count (atom 0)
          function (fn [result]
                     (swap! call-count inc)
                     result)
          cache-atom (create-cache-atom)]

      (is (= :result (call-with-cache cache-atom 0 function :result)))
      (is (= 1 @call-count))

      (is (= :result (call-with-cache cache-atom 0 function :result)))
      (is (= 1 @call-count)))))

(ns fungl.identity-cache
  (:require [clojure.test :refer [deftest is testing]]))

(defn initial-state []
  {})

(defn add-to-cache [cache identity-keys value-keys value]
  (update cache (hash [identity-keys value-keys])
          (fnil conj [])
          [identity-keys value-keys value]))

(deftest test-add-to-cache
  (is (= {83065300 [[[:identity-key] [:value-key] :value]]}
         (add-to-cache {} [:identity-key] [:value-key] :value))))

(defn compare-sequences [comparator sequence-1 sequence-2]
  (and (= (count sequence-1)
          (count sequence-2))
       (loop [sequence-1 sequence-1
              sequence-2 sequence-2]
         (if (and (empty? sequence-1)
                  (empty? sequence-2))
           true
           (if (comparator (first sequence-1)
                           (first sequence-2))
             (recur (rest sequence-1)
                    (rest sequence-2))
             false)))))

(defn get-from-cache [cache identity-keys value-keys]
  (loop [mappings (get cache
                       (hash [identity-keys value-keys]))]
    (if-some [[cached-identity-keys cached-value-keys value] (first mappings)]
      (if (and (compare-sequences identical?
                                  identity-keys
                                  cached-identity-keys)
               (compare-sequences =
                                  value-keys
                                  cached-value-keys))
        value
        (recur (rest mappings)))
      ::not-found)))

(deftest test-get-from-cache
  (is (= :value
         (let [identity-key {:a :b}]
           (-> (initial-state)
               (add-to-cache [identity-key]
                             [{:c :d}]
                             :value)
               (get-from-cache [identity-key]
                               [{:c :d}])))))

  (is (= nil
         (let [identity-key {:a :b}]
           (-> (initial-state)
               (add-to-cache [identity-key]
                             [{:c :d}]
                             nil)
               (get-from-cache [identity-key]
                               [{:c :d}])))))

  (testing "equality is not enough for identity keys"
    (is (= ::not-found
           (-> (initial-state)
               (add-to-cache [{:a :b}]
                             [{:c :d}]
                             :value)
               (get-from-cache [{:a :b}]
                               [{:c :d}]))))))

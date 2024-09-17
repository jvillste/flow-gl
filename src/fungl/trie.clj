(ns fungl.trie
  (:require [clojure.test :refer [deftest is]]))

(defn create-trie []
  {})

(defn update-in-trie [trie path update-function & arguments]
  (if (empty? path)
    (apply update trie ::value update-function arguments)
    (update trie
            (first path)
            (fn [node]
              (apply update-in-trie
                     (or node {})
                     (rest path)
                     update-function
                     arguments)))))

(deftest test-update-in-trie
  (is (= {::value 1}
         (update-in-trie (create-trie)
                         []
                         (constantly 1))))

  (is (= {:a {::value 2}}
         (-> (create-trie)
             (update-in-trie [:a] (constantly 1))
             (update-in-trie [:a] inc)))))

(defn assoc-trie [trie path value]
  (update-in-trie trie path (constantly value)))

(deftest test-assoc-trie
  (is (= {::value :value}
         (assoc-trie (create-trie) [] :value)))

  (is (= {:a {::value :value}}
         (assoc-trie (create-trie) [:a]  :value)))

  (is (= {:a {:b {::value :value}
              ::value :value}}
         (-> (create-trie)
             (assoc-trie [:a] :value)
             (assoc-trie [:a :b] :value))))

  (is (= {:a {:b {::value :value}
              :c {::value :value}
              ::value :value}}
         (-> (create-trie)
             (assoc-trie [:a] :value)
             (assoc-trie [:a :b] :value)
             (assoc-trie [:a :c] :value)))))

(defn get-in-trie [trie path]
  (::value (get-in trie path)))

(deftest test-get-in-trie
  (is (= :value
         (-> (create-trie)
             (assoc-trie [:a :b] :value)
             (get-in-trie [:a :b]))))

  (is (= nil
         (-> (create-trie)
             (assoc-trie [:a :b] :value)
             (get-in-trie [:a])))))

(defn last-value-on-path [trie path]
  (loop [trie trie
         path path]
    (if-some [next-path-token (first path)]
      (if-some [next-trie (get trie next-path-token)]
        (recur next-trie
               (rest path))
        (::value trie))
      (::value trie))))

(deftest test-last-value-on-path
  (is (= :value (last-value-on-path (assoc-trie (create-trie) [] :value)
                                    [])))

  (is (= :value (last-value-on-path (assoc-trie (create-trie) [:a] :value)
                                    [:a])))

  (is (= :value (last-value-on-path (assoc-trie (create-trie) [:a :b] :value)
                                    [:a :b])))

  (is (= :value (last-value-on-path (assoc-trie (create-trie) [:a] :value)
                                    [:a :b])))

  (is (= :value-1 (last-value-on-path (-> (create-trie)
                                          (assoc-trie [:a :b] :value-1)
                                          (assoc-trie [:a :c] :value-2))
                                      [:a :b])))

  (is (= :value-2 (last-value-on-path (-> (create-trie)
                                          (assoc-trie [:a :b] :value-1)
                                          (assoc-trie [:a :b :c] :value-2))
                                      [:a :b :c])))

  (is (= :value-1 (last-value-on-path (-> (create-trie)
                                          (assoc-trie [:a :b] :value-1)
                                          (assoc-trie [:a :b :c] :value-2))
                                      [:a :b])))

  (is (nil? (last-value-on-path (assoc-trie (create-trie) [:a] :value)
                                [])))

  (is (nil? (last-value-on-path (assoc-trie (create-trie) [:a :b :c] :value)
                                [:a :b])))

  (is (nil? (last-value-on-path (assoc-trie (create-trie) [:a] :value)
                                [:b]))))

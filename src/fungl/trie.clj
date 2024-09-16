(ns fungl.trie
  (:require [clojure.test :refer [deftest is]]))

(defn create-trie []
  {})

(defn add-to-trie [trie path value]
  (if (empty? path)
    (assoc trie ::value value)
    (update trie
            (first path)
            (fn [node]
              (add-to-trie (or node {})
                           (rest path)
                           value)))))

(deftest test-add-to-trie
  (is (= {::value :value}
         (add-to-trie {} [] :value)))

  (is (= {:a {::value :value}}
         (add-to-trie {} [:a]  :value)))

  (is (= {:a {:b {::value :value}
              ::value :value}}
         (-> {}
             (add-to-trie [:a] :value)
             (add-to-trie [:a :b] :value))))

  (is (= {:a {:b {::value :value}
              :c {::value :value}
              ::value :value}}
         (-> {}
             (add-to-trie [:a] :value)
             (add-to-trie [:a :b] :value)
             (add-to-trie [:a :c] :value)))))

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
  (is (= :value (last-value-on-path (add-to-trie {} [] :value)
                                    [])))

  (is (= :value (last-value-on-path (add-to-trie {} [:a] :value)
                                    [:a])))

  (is (= :value (last-value-on-path (add-to-trie {} [:a :b] :value)
                                    [:a :b])))

  (is (= :value (last-value-on-path (add-to-trie {} [:a] :value)
                                    [:a :b])))

  (is (= :value-1 (last-value-on-path (-> {}
                                          (add-to-trie [:a :b] :value-1)
                                          (add-to-trie [:a :c] :value-2))
                                      [:a :b])))

  (is (= :value-2 (last-value-on-path (-> {}
                                          (add-to-trie [:a :b] :value-1)
                                          (add-to-trie [:a :b :c] :value-2))
                                      [:a :b :c])))

  (is (= :value-1 (last-value-on-path (-> {}
                                          (add-to-trie [:a :b] :value-1)
                                          (add-to-trie [:a :b :c] :value-2))
                                      [:a :b])))

  (is (nil? (last-value-on-path (add-to-trie {} [:a] :value)
                                [])))

  (is (nil? (last-value-on-path (add-to-trie {} [:a :b :c] :value)
                                [:a :b])))

  (is (nil? (last-value-on-path (add-to-trie {} [:a] :value)
                                [:b]))))

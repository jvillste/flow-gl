(ns fungl.trie
  (:require [clojure.test :refer [deftest is]]))

(defn create-trie []
  {})

(defn add-to-trie [trie sequence]
  (if (empty? sequence)
    (assoc trie ::head true)
    (update trie
            (first sequence)
            (fn [node]
              (add-to-trie (or node {})
                                    (rest sequence))))))

(deftest test-add-to-trie
  (is (= {::head true}
         (add-to-trie {} [])))

  (is (= {:a {::head true}}
         (add-to-trie {} [:a])))

  (is (= {:a {:b {::head true}
              ::head true}}
         (-> {}
             (add-to-trie [:a])
             (add-to-trie [:a :b]))))

  (is (= {:a {:b {::head true}
              :c {::head true}
              ::head true}}
         (-> {}
             (add-to-trie [:a])
             (add-to-trie [:a :b])
             (add-to-trie [:a :c])))))

(defn prefix-in-trie? [trie sequence]
  (loop [trie trie
         sequence sequence]
    (if (::head trie)
      true
      (if-some [value (first sequence)]
        (if-some [next-trie (get trie value)]
          (recur next-trie
                 (rest sequence))
          false)
        false))))

(deftest test-prefix-in-trie?
  (is (prefix-in-trie? (add-to-trie {} [])
                       []))

  (is (prefix-in-trie? (add-to-trie {} [:a])
                       [:a]))

  (is (prefix-in-trie? (add-to-trie {} [:a :b])
                       [:a :b]))

  (is (prefix-in-trie? (add-to-trie {} [:a])
                       [:a :b]))

  (is (prefix-in-trie? (-> {}
                           (add-to-trie [:a :b])
                           (add-to-trie [:a :c]))
                       [:a :b]))

  (is (not (prefix-in-trie? (add-to-trie {} [:a])
                            [])))

  (is (not (prefix-in-trie? (add-to-trie {} [:a :b :c])
                            [:a :b])))

  (is (not (prefix-in-trie? (add-to-trie {} [:a])
                            [:b]))))

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

(defn assoc-in-trie [trie path value]
  (update-in-trie trie path (constantly value)))

(deftest test-assoc-in-trie
  (is (= {::value :value}
         (assoc-in-trie (create-trie) [] :value)))

  (is (= {:a {::value :value}}
         (assoc-in-trie (create-trie) [:a]  :value)))

  (is (= {:a {:b {::value :value}
              ::value :value}}
         (-> (create-trie)
             (assoc-in-trie [:a] :value)
             (assoc-in-trie [:a :b] :value))))

  (is (= {:a {:b {::value :value}
              :c {::value :value}
              ::value :value}}
         (-> (create-trie)
             (assoc-in-trie [:a] :value)
             (assoc-in-trie [:a :b] :value)
             (assoc-in-trie [:a :c] :value)))))

(defn dissoc-in-trie [trie path]
  (if (= 1 (count path))
    (if (contains? trie (first path))
      (if (= [::value] (keys (get trie (first path))))
        (dissoc trie (first path))
        (update trie (first path) dissoc ::value))
      trie)
    (if (contains? trie (first path))
      (let [new-trie (update trie
                             (first path)
                             (fn [node]
                               (dissoc-in-trie
                                node
                                (rest path))))]
        (if (empty? (get new-trie (first path)))
          (dissoc new-trie (first path))
          new-trie))
      trie)))

(deftest test-dissoc-in-trie
  (is (= {}
         (-> (create-trie)
             (assoc-in-trie [1] :foo)
             (dissoc-in-trie [1]))))

  (is (= {1 {1 {::value :foo}}}
         (-> (create-trie)
             (assoc-in-trie [1 1] :foo)
             (dissoc-in-trie [1]))))

  (is (= {1 {1 {::value :foo}}}
         (-> (create-trie)
             (assoc-in-trie [1 1] :foo)
             (dissoc-in-trie [2]))))

  (is (= {1 {1 {::value :foo}}}
         (-> (create-trie)
             (assoc-in-trie [1 1] :foo)
             (dissoc-in-trie [1 2]))))

  (is (= {}
         (-> (create-trie)
             (assoc-in-trie [1 1] :foo)
             (dissoc-in-trie [1 1]))))

  (is (= {1 {1 {::value :foo}}}
         (-> (create-trie)
             (assoc-in-trie [1 1] :foo)
             (assoc-in-trie [1 2] :bar)
             (dissoc-in-trie [1 2]))))

  (is (= {}
         (-> (create-trie)
             (assoc-in-trie [1 1] :foo)
             (assoc-in-trie [1 2] :bar)
             (dissoc-in-trie [1 2])
             (dissoc-in-trie [1 1])))))

(defn get-in-trie [trie path]
  (::value (get-in trie path)))

(deftest test-get-in-trie
  (is (= :value
         (-> (create-trie)
             (assoc-in-trie [:a :b] :value)
             (get-in-trie [:a :b]))))

  (is (= nil
         (-> (create-trie)
             (assoc-in-trie [:a :b] :value)
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
  (is (= :value (last-value-on-path (assoc-in-trie (create-trie) [] :value)
                                    [])))

  (is (= :value (last-value-on-path (assoc-in-trie (create-trie) [:a] :value)
                                    [:a])))

  (is (= :value (last-value-on-path (assoc-in-trie (create-trie) [:a :b] :value)
                                    [:a :b])))

  (is (= :value (last-value-on-path (assoc-in-trie (create-trie) [:a] :value)
                                    [:a :b])))

  (is (= :value-1 (last-value-on-path (-> (create-trie)
                                          (assoc-in-trie [:a :b] :value-1)
                                          (assoc-in-trie [:a :c] :value-2))
                                      [:a :b])))

  (is (= :value-2 (last-value-on-path (-> (create-trie)
                                          (assoc-in-trie [:a :b] :value-1)
                                          (assoc-in-trie [:a :b :c] :value-2))
                                      [:a :b :c])))

  (is (= :value-1 (last-value-on-path (-> (create-trie)
                                          (assoc-in-trie [:a :b] :value-1)
                                          (assoc-in-trie [:a :b :c] :value-2))
                                      [:a :b])))

  (is (nil? (last-value-on-path (assoc-in-trie (create-trie) [:a] :value)
                                [])))

  (is (nil? (last-value-on-path (assoc-in-trie (create-trie) [:a :b :c] :value)
                                [:a :b])))

  (is (nil? (last-value-on-path (assoc-in-trie (create-trie) [:a] :value)
                                [:b]))))



(defn value-count [trie]
  (+ (if (::value trie)
       1 0)
     (->> (dissoc trie ::value)
          (vals)
          (filter map?)
          (map value-count)
          (apply +))))

(deftest test-value-count
  (is (= 2
         (value-count (-> (create-trie)
                          (assoc-in-trie [:a :b] :value-1)
                          (assoc-in-trie [:a :b :c] :value-2))))))


(defn do-values
  ([function trie]
   (do-values function trie []))

  ([function trie path]
   (when-some [value (::value trie)]
     (function path value))
   (loop [keys (remove #{::value}
                       (keys trie))]
     (when-let [key (first keys)]
       (do-values function
                  (get trie key)
                  (conj path key))
       (recur (rest keys))))))

(defn values [trie]
  (let [values-atom (atom {})]
    (do-values (fn [path value]
                 (swap! values-atom assoc path value))
               trie)
    @values-atom))

(deftest test-values
  (is (= {[:a] :value-1
          [:a :b] :value-2}
         (values (-> (create-trie)
                     (assoc-in-trie [:a] :value-1)
                     (assoc-in-trie [:a :b] :value-2))))))

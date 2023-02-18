(ns flow-gl.utils
  (:refer-clojure :exclude [get])
  (:require [clojure.test :refer [deftest is]]))

#_(let [log-file-name "/Users/jukka/Downloads/versio2.txt"]
    (spit log-file-name "")
    (defn log-to-file [& values]
      (spit log-file-name (apply str (interpose " " values)) :append true)))

(defn map-vals [m f]
  (zipmap (keys m) (map f (vals m))))

(defmacro named-time [name expression]
  `(do (println ~name)
       (time ~expression)))

(defn get-and-reset [atom key new-value]
  (let [now-key (keyword (namespace key) (str (name key) "-now"))]
    (swap! atom (fn [value]
                  (assoc value now-key (key value)
                         key new-value)))))

#_(fact get-and-reset-test
        (get-and-reset (atom {::foo 4})
                       ::foo
                       0)
        =>
        {::foo-now 4, ::foo 0})

(defmacro for-all [bindings body]
  `(doall (for ~bindings ~body)))

(defn map-all-indexed [function coll]
  (doall (map-indexed function coll)))



(defn in?
  "true if seq contains elm"
  [elm seq]
  (some #(= elm %) seq))

(def ^:dynamic delayed-applications {})

(defn with-delayed-applications-function [key value function]
  (binding [delayed-applications (assoc delayed-applications key (atom []))]
    (let [value (function value)]
      (reduce (fn [value function]
                (function value))
              value
              @(key delayed-applications)))))

(defmacro with-delayed-applications [key value & body]
  `(with-delayed-applications-function ~key ~value (fn [~value] ~@body)))

(defn apply-later [key function]
  (if (contains? delayed-applications key)
    (swap! (key delayed-applications) conj function)
    (throw (Exception. (str "No delayed applications available for the key " key)))))


;; Tests


#_(fact delayed-applications-with-one-var
        (let [x {}]
          (with-delayed-applications :x x
            (assoc x :foo (do
                            (apply-later :x
                                         (fn [x] (assoc x :x-bar :x-bar-value)))

                            :foo-value))))

        => {:foo :foo-value
            :x-bar :x-bar-value})

#_(fact apply-later-for-wrong-key-throws-exception
        (let [x 1]
          (with-delayed-applications :x x
            (apply-later :y
                         (fn [x]))))
        => (throws Exception "No delayed applications available for the key :y"))

#_(fact delayed-applications-with-two-vars
        (let [x {}
              y {}]
          (with-delayed-applications :x x
            (assoc x :x-foo (with-delayed-applications :y y
                              (assoc y :y-foo (do
                                                (apply-later :x
                                                             (fn [x] (assoc x :x-bar :x-bar-value)))
                                                (apply-later :y
                                                             (fn [y] (assoc y :y-bar :y-bar-value)))
                                                :y-foo-value))))))
        => {:x-bar :x-bar-value
            :x-foo {:y-bar :y-bar-value
                    :y-foo :y-foo-value}})

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

(defn print-return [message value]
  (println message value)
  value)

(defn insert [vector index value]
  (apply conj (vec (take index vector)) value (drop index vector)))

#_(fact (insert [1 2 3 4] 1 10) => [1 10 2 3 4])

(defn remove-nth [vector index]
  (vec (concat (take index vector) (drop (+ index 1) vector))))

#_(fact (remove [1 2 3 4] 1) => [1 3 4])

(defn partial-rest [function & arguments]
  (fn [value]
    (apply function
           value
           arguments)))

(deftest test-partial-rest
  (is (= "012"
         ((partial-rest str 1 2)
          0))))

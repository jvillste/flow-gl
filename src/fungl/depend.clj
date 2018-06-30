(ns fungl.depend
  (:require [clojure.test :refer :all]))

(defmulti dependency-added :type)
(defmulti current-value :type)

(def ^:dynamic dependencies [])

(defn with-dependencies-impl [function]
  (binding [dependencies (conj dependencies (atom {}))]
    (let [result (function)]
      (when (< 1 (count dependencies))
        (swap! (last (drop-last dependencies))
               conj @(last dependencies)))
      result)))

(defmacro with-dependencies [& body]
  `(with-dependencies-impl (fn [] ~@body)))

(defn add-dependency [dependency value]

  (when-let [current-dependencies (last dependencies)]
    (do (dependency-added dependency)
        (swap! current-dependencies
               assoc dependency value))))

(defn current-dependencies []
  @(last dependencies))


(deftest dependency-test
  (with-dependencies
    (add-dependency :foo 1)
    (with-dependencies
      (add-dependency :bar 2)
      (is (= {:bar 2}
             (current-dependencies))))
    
    (is (= {:bar 2
            :foo 1}
           (current-dependencies))))

  (add-dependency :baz 1))






#_(defn deref-depended-atom [depended-atom]
    (add-dependency))

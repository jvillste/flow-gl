(ns fungl.depend
  (:use clojure.test))

(def ^:dynamic dependencies [])

(defn with-dependencies-impl [function]
  (binding [dependencies (conj dependencies (atom {}))]
    (function)
    (when (< 1 (count dependencies))
      (swap! (last (drop-last dependencies))
             conj @(last dependencies)))))

(defmacro with-dependencies [& body]
  `(with-dependencies-impl (fn [] ~@body)))

(defn add-dependency [key value]
  (when-let [current-dependencies (last dependencies)]
    (swap! current-dependencies
           assoc key value)))

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

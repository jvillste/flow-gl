(ns fungl.callable
  (:require [clojure.test :refer [deftest is]]))

(defn call [callable & arguments]
  (cond (vector? callable)
        (apply (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply callable arguments)))

(defn call-arguments-first [callable & arguments]
  (cond (vector? callable)
        (apply (first callable)
               (concat arguments
                       (rest callable)))

        (fn? callable)
        (apply callable arguments)))

(defn callable? [value]
  (or (fn? value)
      (and (var? value)
           (fn? @value))
      (and (vector? value)
           (or (fn? (first value))
               (and (var? (first value))
                    (fn? @(first value)))))))

(deftest test-callable?
  (is (callable? inc))
  (is (callable? #'inc))
  (is (callable? [inc]))
  (is (callable? [#'inc]))

  (is (not (callable? [1]))))

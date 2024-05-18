(ns fungl.callable
  (:require [clojure.test :refer [deftest is]]
            [fungl.cache :as cache]
            [fungl.identity-cache :as identity-cache]))

(defn call [callable & arguments]
  (cond (vector? callable)
        (apply (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply callable arguments)))

(defn call-with-cache [callable & arguments]
  (cond (vector? callable)
        (apply cache/call!
               (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply cache/call! callable arguments)))

(defn call-with-identity-cache [cache-atom number-of-identity-arguments callable & arguments]
  (cond (vector? callable)
        (apply identity-cache/call-with-cache
               cache-atom
               number-of-identity-arguments
               (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply identity-cache/call-with-cache
               cache-atom
               number-of-identity-arguments
               callable
               arguments)))

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

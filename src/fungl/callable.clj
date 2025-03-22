(ns fungl.callable
  (:require [clojure.test :refer [deftest is]]
            [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(defn call [callable & arguments]
  (cond (vector? callable)
        (apply (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply callable arguments)))

(defn call-with-hierarchical-identity-cache [cache-atom path number-of-identity-arguments callable & arguments]
  (cond (vector? callable)
        (apply hierarchical-identity-cache/call-with-cache
               cache-atom
               path
               number-of-identity-arguments
               (first callable)
               (concat (rest callable)
                       arguments))

        (fn? callable)
        (apply hierarchical-identity-cache/call-with-cache
               cache-atom
               path
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

(defn function [callable]
  (if (vector? callable)
    (first callable)
    callable))

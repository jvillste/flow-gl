(ns fungl.id-comparator
  (:require [clojure.test :use [deftest is]]))

;; from https://clojure.org/guides/comparators

;; comparison-class throws exceptions for some types that might be
;; useful to include.

(defn comparison-class [x]
  (cond (nil? x) ""
        ;; Lump all numbers together since Clojure's compare can
        ;; compare them all to each other sensibly.
        (number? x) "java.lang.Number"

        ;; sequential? includes lists, conses, vectors, and seqs of
        ;; just about any collection, although it is recommended not
        ;; to use this to compare seqs of unordered collections like
        ;; sets or maps (vectors should be OK).  This should be
        ;; everything we would want to compare using cmp-seq-lexi
        ;; below.  TBD: Does it leave anything out?  Include anything
        ;; it should not?
        (sequential? x) "clojure.lang.Sequential"

        (set? x) "clojure.lang.IPersistentSet"
        (map? x) "clojure.lang.IPersistentMap"
        (.isArray (class x)) "java.util.Arrays"

        ;; Comparable includes Boolean, Character, String, Clojure
        ;; refs, and many others.
        (instance? Comparable x) (.getName (class x))
        :else (throw
               (ex-info (format "cc-cmp does not implement comparison of values with class %s"
                                (.getName (class x)))
                        {:value x}))))

(defn cmp-seq-lexi
  [cmpf x y]
  (loop [x x
         y y]
    (if (seq x)
      (if (seq y)
        (let [c (cmpf (first x) (first y))]
          (if (zero? c)
            (recur (rest x) (rest y))
            c))
        ;; else we reached end of y first, so x > y
        1)
      (if (seq y)
        ;; we reached end of x first, so x < y
        -1
        ;; Sequences contain same elements.  x = y
        0))))

;; The same result can be obtained by calling cmp-seq-lexi on two
;; vectors, but cmp-vec-lexi should allocate less memory comparing
;; vectors.
(defn cmp-vec-lexi
  [cmpf x y]
  (let [x-len (count x)
        y-len (count y)
        len (min x-len y-len)]
    (loop [i 0]
      (if (== i len)
        ;; If all elements 0..(len-1) are same, shorter vector comes
        ;; first.
        (compare x-len y-len)
        (let [c (cmpf (x i) (y i))]
          (if (zero? c)
            (recur (inc i))
            c))))))

(defn cmp-array-lexi
  [cmpf x y]
  (let [x-len (alength x)
        y-len (alength y)
        len (min x-len y-len)]
    (loop [i 0]
      (if (== i len)
        ;; If all elements 0..(len-1) are same, shorter array comes
        ;; first.
        (compare x-len y-len)
        (let [c (cmpf (aget x i) (aget y i))]
          (if (zero? c)
            (recur (inc i))
            c))))))

(defn compare-ids
  [x y]
  (let [x-cls (comparison-class x)
        y-cls (comparison-class y)
        c (compare x-cls y-cls)]
    (cond (not= c 0) c                  ; different classes

          ;; Compare sets to each other as sequences, with elements in
          ;; sorted order.
          (= x-cls "clojure.lang.IPersistentSet")
          (cmp-seq-lexi compare-ids (sort compare-ids x) (sort compare-ids y))

          ;; Compare maps to each other as sequences of [key val]
          ;; pairs, with pairs in order sorted by key.
          (= x-cls "clojure.lang.IPersistentMap")
          (cmp-seq-lexi compare-ids
                        (sort-by key compare-ids (seq x))
                        (sort-by key compare-ids (seq y)))

          (= x-cls "java.util.Arrays")
          (cmp-array-lexi compare-ids x y)

          ;; Make a special check for two vectors, since cmp-vec-lexi
          ;; should allocate less memory comparing them than
          ;; cmp-seq-lexi.  Both here and for comparing sequences, we
          ;; must use compare-ids recursively on the elements, because if
          ;; we used compare we would lose the ability to compare
          ;; elements with different types.
          (and (vector? x) (vector? y)) (cmp-vec-lexi compare-ids x y)

          ;; This will compare any two sequences, if they are not both
          ;; vectors, e.g. a vector and a list will be compared here.
          (= x-cls "clojure.lang.Sequential")
          (cmp-seq-lexi compare-ids x y)

          :else (compare x y))))

(defmacro run-compare-ids-test [collection]
  `(is (= ~collection (sort-by identity
                               compare-ids
                               ~collection))))

(deftest test-compare-ids

  (run-compare-ids-test [[1 :name 1 :set "Name 1"]
                         [1 :name 2 :set "Name 1"]])

  (run-compare-ids-test [[1 :name nil nil nil]
                         [1 :name 2 :set "Name 1"]])


  (run-compare-ids-test [[1]
                         [1 0]
                         [2]
                         [2 0]])
  1
  (run-compare-ids-test [[1]
                         [1 :call]
                         [2]
                         [2 0]]))

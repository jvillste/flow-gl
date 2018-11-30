(ns fungl.util
  (:require [clojure.test :refer :all]))

(defn options-map-to-destructuring [options-map]
  {:keys (vec (keys options-map))
   :or options-map})

(defn options-map-to-arg-list [[arguments options-map & body]]
  (concat (list (conj arguments
                      (options-map-to-destructuring options-map)))
          body))

(deftest test-options-map-to-arg-list
  (is (= '([x y {:keys [z], :or {z 0}}] (+ x y z))
         (options-map-to-arg-list '([x y] {z 0} (+ x y z))))))

(defn arity-without-options-map [name fixed-arguments]
  (list fixed-arguments
        (concat (list name)
                (conj fixed-arguments {}))))

(deftest test-arity-without-options-map
  (is (= '([x y] (foo x y {}))
         (arity-without-options-map 'foo '[x y]))))

(defn parse-function-declaration [name function-declaration]
  (let [metadata (if (string? (first function-declaration))
                   {:doc (first function-declaration)}
                   {})
        function-declaration (if (string? (first function-declaration))
                               (next function-declaration)
                               function-declaration)
        metadata (if (map? (first function-declaration))
                   (conj metadata (first function-declaration))
                   metadata)
        function-declaration (if (map? (first function-declaration))
                               (next function-declaration)
                               function-declaration)
        fixed-arguments (first function-declaration)
        _ (assert (map? (second function-declaration))
                  "argument list must be followed by options map definition")
        function-declaration (list (arity-without-options-map name fixed-arguments)
                                   (options-map-to-arg-list function-declaration))
        metadata (conj {:arglists (list 'quote (#'clojure.core/sigs function-declaration))} metadata)
        metadata (conj (if (meta name) (meta name) {}) metadata)]
    [metadata function-declaration]))

(deftest test-parse-function-declaration
  (is (= '[{:arglists '([x y]
                        [x y {:keys [z], :or {z 0}}])}
           (([x y] (name x y {}))
            ([x y {:keys [z], :or {z 0}}] (+ x y z)))]

         (parse-function-declaration 'name
                                     '([x y]
                                       {z 0}
                                       (+ x y z)))))

  (testing "name symbols metadata is copied to the parsed metadata"
    (let [[metadata _] (parse-function-declaration (with-meta 'name {:private true})
                                                   '([x y]
                                                     {z 0}
                                                     (+ x y z)))]

      (is (:private metadata))))

  (testing "docstring is parsed to the metadata"
    (let [[metadata _] (parse-function-declaration 'name
                                                   '("this is doc string"
                                                     [x y]
                                                     {z 0}
                                                     (+ x y z)))]

      (is (= "this is doc string", (:doc metadata)))))

  (testing "metadata attributes are parsed to the metadata"
    (let [[metadata _] (parse-function-declaration 'name
                                                   '({:private true}
                                                     [x y]
                                                     {z 0}
                                                     (+ x y z)))]

      (is (:private metadata))))

  (testing "docstring and metadata attributes are parsed to the metadata"
    (let [[metadata _] (parse-function-declaration 'name
                                                   '("this is doc string"
                                                     {:private true}
                                                     [x y]
                                                     {z 0}
                                                     (+ x y z)))]

      (is (= "this is doc string", (:doc metadata)))
      (is (:private metadata)))))

(defmacro defno
  "Same as defn but argument list is followed by an optoins map definition formatted as a map of default values
  like in map destructuring. A function with two arities is defined. One takes an options map and one does not.

  Example definition: (defno add-to-y [x] {y 0} (+ x y))
  Example usages: (add-to-y 1) (add-to-y 1 {y 3})"
  {:arglists '([name doc-string? attribute-map? [params*] options-map body])}
  [name & function-declaration]
  (assert (instance? clojure.lang.Symbol name)
          "First argument to defno must be a symbol")
  (let [[metadata function-declaration] (parse-function-declaration name function-declaration)]
    (list 'def (with-meta name metadata)
          (with-meta (cons 'fn function-declaration)
            {:rettag (:tag metadata)}))))



;;;; Test

(defno ^:private foo
  "hello"
  {:private true}
  [x y]
  {z 0}
  (+ x y z))

(deftest test-foo
  (is (:private (meta #'foo)))
  (is (= "hello" (:doc (meta #'foo))))
  (is (= 3 (foo 1 2)))
  (is (= 4 (foo 1 2 {:z 1}))))

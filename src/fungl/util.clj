(ns fungl.util
  (:require [clojure.test :refer :all]
            [clojure.walk :as walk]))

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

(defn spy [name value]
  (prn name value)
  value)

(defn value-size
  ([value]
   (value-size 20 value))
  ([maximum value]
   (loop [values [value]
          size 0]
     (if (< maximum size)
       size
       (if (empty? values)
         size
         (let [value (first values)
               value (if (instance? clojure.lang.IDeref value)
                       (deref value)
                       value)]
           (if (seqable? value)
             (recur (concat (rest values)
                            (seq value))
                    (inc size))
             (recur (rest values)
                    (inc size)))))))))

(deftest test-value-size
  (is (= 1
         (value-size :a)))

  (is (= 4
         (value-size {:a :b})))

  (is (= 6
         (value-size {:a [:b :c]})))

  (is (= 4
         (value-size (atom {:a :b}))))

  (is (= 5
         (value-size [(atom {:a :b})])))

  (is (= 3
         (value-size [false true])))

  (is (= 3
         (value-size [nil 1])))

  (is (= 21
         (value-size (range 100))))

  (is (= 6
         (value-size 5 (range 100)))))


;; originally from https://github.com/trhura/clojure-term-colors/blob/master/src/clojure/term/colors.clj
(defn- escape-code
  [i]
  (str "\u001b[" i "m"))

(def escape (merge (zipmap [:grey :red :green :yellow
                            :blue :magenta :cyan :white]
                           (map escape-code
                                (range 30 38)))
                   (zipmap [:on-grey :on-red :on-green :on-yellow
                            :on-blue :on-magenta :on-cyan :on-white]
                           (map escape-code
                                (range 40 48)))
                   (into {}
                         (filter (comp not nil? key)
                                 (zipmap [:bold, :dark, nil, :underline,
                                          :blink, nil, :reverse-color, :concealed]
                                         (map escape-code (range 1 9)))))
                   {:reset (escape-code 0)}))

(defn escapes [& keys]
  (apply str (map escape keys)))


;;;; Test

(defno foo
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


(comment

  (defno bar [a] [d 0
                  :as options FooOptions]
    (+ d (foo a options)))


  (defno baz [a e] [:as options BarOptions]
    (+ e (bar a options))))

(defn starts-with? [sequence-1 sequence-2]
  (= (take (count sequence-1)
           sequence-2)
     sequence-1))

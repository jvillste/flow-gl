(ns flow-gl.thread-inheritable
  (:import (clojure.lang IDeref)))

;; From http://aphyr.com/posts/240-configuration-and-scope

(defn thread-inheritable
  "Creates a dynamic, thread-local, thread-inheritable object, with initial
  value 'value'. Set with (.set x value), read with (deref x)."
  [value]
  (doto (proxy [InheritableThreadLocal IDeref] []
          (deref [] (.get this)))
    (.set value)))

(defn set-dynamic-thread-vars!
  "Takes a map of vars to values, and assigns each."
  [bindings-map]
  (doseq [[v value] bindings-map]
    (.set v value)))

(defmacro inheritable-binding 
  "Creates new bindings for the (already-existing) dynamic thread-inherited
  vars, with the supplied initial values. Executes exprs in an implict do, then
  re-establishes the bindings that existed before. Bindings are made
  sequentially, like let."
  [bindings & body]
  `(let [inner-bindings# (hash-map ~@bindings)
         outer-bindings# (into {} (for [[k# v#] inner-bindings#]
                                        [k# (deref k#)]))]
    (try
      (set-dynamic-thread-vars! inner-bindings#)
       ~@body
       (finally
         (set-dynamic-thread-vars! outer-bindings#)))))

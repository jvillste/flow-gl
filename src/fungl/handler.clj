(ns fungl.handler
  (:require  (fungl [cache :as cache])))

(defmacro def-handler-creator
  "For example calling:

   (def-handler-creator create-number-handler [x] [y]
     (+ x y))

   and then calling (create-number-handler 1) always returns
   the same function (fn [y] (+ x y)) with a context where x is 1.
   Calling (create-number-handler 2) returns similar function,
   but with a context where x is 2."
  
  [name fixed-arguments handler-arguments & body]
  (let [implementation-name (symbol (str name "-creator"))]
    `(do (defn ~implementation-name ~fixed-arguments
           (fn ~handler-arguments
             ~@body))
         (defn ~name ~fixed-arguments
           (cache/call! ~implementation-name ~@fixed-arguments)))))

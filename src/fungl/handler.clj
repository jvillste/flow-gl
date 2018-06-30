(ns fungl.handler
  (:require [clojure.test :refer :all]
            [fungl.value-registry :as value-registry]
            [fungl.cache :as cache]))

(defmacro def-handler-creator
  "DEPRECATED: use value-registry/getfn! instead

   For example calling:

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


(defmacro create-handler-with-fixed-context
  "DEPRECATED: use value-registry/getfn! instead"
  
  [id fixed-arguments handler-arguments & body]
  `(value-registry/get! [~id ~@fixed-arguments] {:create (fn []
                                                           (fn [~@handler-arguments]
                                                             ~@body))}))

(deftest test-create-handler-with-fixed-context
  (with-bindings (value-registry/state-bindings)
    (let [x 1]
      (is (= (create-handler-with-fixed-context :text-area-keyboard-event-handler [x] [y] [x y])
             (create-handler-with-fixed-context :text-area-keyboard-event-handler [x] [y] [x y]))))

    (let [x 1]
      (let [first-handler (create-handler-with-fixed-context :text-area-keyboard-event-handler [x] [y] [x y])]
        (let [x 2]
          (is (not= first-handler
                    (create-handler-with-fixed-context :text-area-keyboard-event-handler [x] [y] [x y]))))))))




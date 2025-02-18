(ns fungl.derivation
  (:require [fungl.depend :as depend]
            [fungl.cache :as cache]
            clojure.pprint))

(defrecord Derivation [name function]
  clojure.lang.IDeref
  (deref [this]
    (let [result (depend/with-hidden-dependencies (cache/call! function))]
      (depend/add-dependency this
                             result)
      result))

  clojure.lang.Named
  (getName [this]
    (:name this))

  depend/Depend
  (current-value [_this]
    (cache/call! function)))

(defn derive
  ([function]
   (derive nil function))
  ([name function]
   (Derivation. name function)))

(defmacro def-derivation [name & function-body]
  `(def ~name (derive (str (quote ~name))
                      (fn []
                        ~@function-body))))

(defmethod print-method Derivation [derivation, ^java.io.Writer writer]
  (.write writer "#derivation[")
  (.write writer (pr-str (:name derivation)))
  (.write writer " ")
  (print-method @derivation writer)
  (.write writer (str " "
                      (format "0x%x"
                              (System/identityHashCode derivation))
                      "]")))

;; (prefer-method clojure.pprint/simple-dispatch
;;                Derivation
;;                clojure.lang.IDeref)

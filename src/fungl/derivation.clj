(ns fungl.derivation
  (:require [fungl.depend :as depend]
            [fungl.cache :as cache]))

(defrecord Derivation [name function]
  clojure.lang.IDeref
  (deref [_this]
    (let [result (depend/with-hidden-dependencies (cache/call! function))]
      (depend/add-dependency {:type ::type
                              :function function
                              :name name}
                             result)
      result))

  clojure.lang.Named
  (getName [_this]
    name))

(defn derive
  ([function]
   (derive nil function))
  ([name function]
   (Derivation. name function)))

(defmethod depend/current-value ::type [derivation]
  (cache/call! (:function derivation)))

(defmethod depend/dependency-added ::type [_derivation])

(defmethod print-method Derivation [derivation, ^java.io.Writer writer]
  (.write writer "#derivation[")
  (print-method @derivation writer)
  (.write writer (str " "
                      (format "0x%x"
                              (System/identityHashCode derivation))
                      "]")))

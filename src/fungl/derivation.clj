(ns fungl.derivation
  (:refer-clojure :exclude [derive])
  (:require [fungl.depend :as depend]
            [fungl.cache :as cache]
            clojure.pprint))

(defrecord Derivation [name function]
  clojure.lang.IDeref
  (deref [this]
    ;; TODO: add dependency tracking to identity-cache and use it here.
    (let [old-cached-value (cache/get (cache/function-call-key function []))
          new-value (depend/with-hidden-dependencies (cache/call! function))
          debug-print? false #_(= "focused-node-id-derivation" name)]
      ;; (when (and (not (= old-cached-value new-value))
      ;;            (= "focused-subtrees-with-command-sets" name))
      ;;   (def the-difference (clojure.data/diff old-cached-value
      ;;                                          new-value)))
      (depend/add-dependency this
                             new-value)
      (cond (identical? old-cached-value
                        new-value)
            (do #_(when debug-print?
                    (println "derivation" name "evaluated to identical value"))
                old-cached-value)

            (= old-cached-value
               new-value)
            (do #_(when debug-print?
                    (println "derivation" name "evaluated to equal value"))
                (cache/put! (cache/function-call-key function []) old-cached-value)
                old-cached-value)

            :else
            (do (when debug-print?
                  (println "derivation" name "evaluated to new value"))
                new-value))))

  clojure.lang.Named
  (getName [this]
    (:name this))

  depend/Depend
  (current-value [this]
    (deref this)))

(defn derive
  ([function]
   (derive nil function))
  ([name function]
   (Derivation. name function)))

(defmacro def-derivation [name & function-body]
  `(def ~name (derive (str (quote ~name))
                      (fn ~name []
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

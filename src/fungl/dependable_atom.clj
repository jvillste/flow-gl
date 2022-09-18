(ns fungl.dependable-atom
  (:require [fungl.depend :as depend]
            [clojure.pprint :as pprint]
            [clojure.string :as string])
  (:refer-clojure :exclude [atom])
  (:import java.io.Writer))

(defrecord Atom [name atom]
  clojure.lang.IDeref
  (deref [this]
    (depend/add-dependency this
                           @atom)
    (deref atom))

  clojure.lang.Named
  (getName [_this]
    name)

  clojure.lang.IAtom
  (swap [_this fn]
    (swap! atom fn))

  (swap [_this fn arg]
    (swap! atom fn arg))

  (swap [_this fn arg1 arg2]
    (swap! atom fn arg1 arg2))

  (swap [_this fn arg1 arg2 args]
    (apply swap! atom fn arg1 arg2 args))

  (compareAndSet [_this old-value new-value]
    (compare-and-set! atom old-value new-value))

  (reset [_this new-value]
    (reset! atom new-value))

  clojure.lang.IAtom2

  (swapVals [_this f]
    (swap-vals! atom f))

  (swapVals [_this f arg]
    (swap-vals! atom f arg))

  (swapVals [_this f arg1 arg2]
    (swap-vals! atom f arg1 arg2))

  (swapVals [_this f arg1 arg2 args]
    (swap-vals! atom f arg1 arg2 args))

  (resetVals [_this value]
    (reset-vals! atom value))

  depend/Depend
  (current-value [_this]
    @atom))

(defmethod print-method Atom [dependable-atom, ^java.io.Writer writer]
  (.write writer "#dependable-atom[")
  (.write writer (pr-str (:name dependable-atom)))
  (.write writer " ")
  (print-method @dependable-atom writer)
  (.write writer (str " "
                      (format "0x%x"
                              (System/identityHashCode dependable-atom))
                      "]")))

(defmethod pprint/simple-dispatch Atom [dependable-atom]
  (print "#dependable-atom[")
  (print (pr-str (:name dependable-atom)))
  (print " ")
  (print (string/trim (with-out-str (pprint/pprint @dependable-atom))))
  (print (str " "
              (format "0x%x"
                      (System/identityHashCode dependable-atom))
              "]")))

(defn atom
  ([value]
   (atom nil value))
  ([name value]
   (->Atom name (clojure.core/atom value))))

(defmacro def-named-atom [name value]
  `(def ~name (->Atom ~(clojure.core/name name)
                      (clojure.core/atom ~value))))

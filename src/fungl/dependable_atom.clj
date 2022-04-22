(ns fungl.dependable-atom
  (:require [fungl.depend :as depend])
  (:refer-clojure :exclude [atom])
  (:import java.io.Writer))

(defrecord Atom [atom]
  clojure.lang.IDeref
  (deref [_this]
    (depend/add-dependency {:type ::type
                            :atom atom}
                           @atom)
    (deref atom))

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
    (reset-vals! atom value)))

  (defmethod print-method Atom [dependable-atom, ^java.io.Writer writer]
    (.write writer "#dependable-atom[")
    (print-method @dependable-atom writer)
    (.write writer (str " "
                        (format "0x%x"
                                (System/identityHashCode dependable-atom))
                        "]")))

  (defn atom [value]
    (->Atom (clojure.core/atom value)))

  (defmethod depend/current-value ::type [dependency]
    @(:atom dependency))

  (defmethod depend/dependency-added ::type [dependency])

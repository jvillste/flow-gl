(ns fungl.dependable-atom
  (:require [fungl.depend :as depend])
  (:refer-clojure :exclude [atom])
  (:import java.io.Writer))

(defrecord Atom [atom]
  clojure.lang.IDeref
  (deref [this]
    (depend/add-dependency {:type ::type
                            :atom atom}
                           @atom)
    (deref atom))

  clojure.lang.IAtom
  (swap [this fn]
    (swap! atom fn))

  (swap [this fn arg]
    (swap! atom fn arg))

  (swap [this fn arg1 arg2]
    (swap! atom fn arg1 arg2))

  (swap [this fn arg1 arg2 args]
    (apply swap! atom fn arg1 arg2 args))

  (compareAndSet [this old-value new-value]
    (compare-and-set! atom old-value new-value))

  (reset [this new-value]
    (reset! atom new-value)))

(defmethod print-method Atom [dependable-atom, ^java.io.Writer writer]
  (print-method @dependable-atom writer))

(defn atom [value]
  (->Atom (clojure.core/atom value)))

(defmethod depend/current-value ::type [dependency]
  @(:atom dependency))

(defmethod depend/dependency-added ::type [dependency])

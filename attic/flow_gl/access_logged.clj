(ns flow-gl.access-logged
  (:import (clojure.lang IDeref)))

(defprotocol Accessible
  (accessed? [this]))

(deftype LoggedReference [value is-accessed-atom]
  Accessible
  (accessed? [this] @is-accessed-atom)

  IDeref
  (deref [this]
    (reset! is-accessed-atom true)
    value))

(defn access-logged [value]
  (LoggedReference. value (atom false)))

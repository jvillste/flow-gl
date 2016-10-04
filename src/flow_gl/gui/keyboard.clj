(ns flow-gl.gui.keyboard
  (:require [clojure.spec :as spec]
            [clojure.test :as test :refer [deftest is]]))


(def ^:dynamic keyboard-state-atom)

(defn initialize-keybaord-state []
  (atom {}))

(defmacro with-keyboard-state [keyboard-state & body]
  `(binding [keyboard-state-atom ~keyboard-state]
     ~@body))

(defn call-if-set [handler-key & arguments]
  (when-let [handler (handler-key @keyboard-state-atom)]
    (apply handler arguments)))

(defn set-focused-keyboard-event-handler [keyboard-event-handler]
  (call-if-set :focused-handler {:type :focus-lost})
  
  (swap! keyboard-state-atom
         assoc :focused-handler keyboard-event-handler)
  
  (keyboard-event-handler {:type :focus-gained}))

(defn set-prepending-keyboard-event-handler [keyboard-event-handler]
  (swap! keyboard-state-atom assoc :prepending-handler keyboard-event-handler))

(defn set-appending-keyboard-event-handler [keyboard-event-handler]
  (swap! keyboard-state-atom assoc :appending-handler keyboard-event-handler))

(defn call-focused-keyboard-event-handler [event]
  #_(call-if-set :prepending-handler event)
  (call-if-set :focused-handler event)
  #_(call-if-set :appending-handler event))

(defn set-focused-keyboard-event-hanlder-on-mouse-clicked [keyboard-event-handler event]
  (if (= :mouse-clicked
         (:type event))
    (set-focused-keyboard-event-handler keyboard-event-handler))
  event)

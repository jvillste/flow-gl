(ns flow-gl.gui.component
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as spec-test]
            [flow-gl.gui.scene-graph :as scene-graph]
            [clojure.test :as test :refer [deftest is]]
            [flow-gl.gui.keyboard :as keyboard]
            [flow-gl.utils :as utils]))

(def ^:dynamic component-state-atom)

(defn initialize-state []
  (atom {}))

(defmacro with-component-state [component-state & body]
  `(binding [component-state-atom ~component-state]
     ~@body))

(defn swap-component-state! [id function & arguments]
  (prn @component-state-atom)
  (swap! component-state-atom
         update-in
         [id]
         (fn [component-state]
           (apply function
                  component-state
                  arguments))))

(defn component-state [id]
  (get @component-state-atom id))

(defn give-component-focus [component-id component-keyboard-event-handler]
  (do (swap! component-state-atom assoc :component-in-focus component-id)
      (keyboard/set-focused-event-handler (fn [keyboard-event]
                                             (swap-component-state! component-id
                                                                    component-keyboard-event-handler
                                                                    keyboard-event)))))

(defn component-in-focus []
  (:component-in-focus @component-state-atom))

(defn give-component-focus-on-mouse-click [node mouse-event]
  (if (= :mouse-clicked
         (:type mouse-event))
    (give-component-focus (:id node) (:keyboard-event-handler node)))

  mouse-event)



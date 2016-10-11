(ns flow-gl.gui.stateful)


(defn initialize-state []
  {})



(defn apply-to-stateful-state [state id function & arguments]
  (update-in state
             [id]
             (fn [stateful-state]
               (apply function
                      stateful-state
                      arguments))))

(defn stateful-state [state id]
  (get state id))

(defn call-with-state [state-atom initialize-state create-scene-graph id & arguments]
  (when (not (contains? @state-atom id))
    (swap! state-atom
           assoc id (initialize-state)))

  (apply create-scene-graph
         (get @state-atom id) id arguments))

;; dynamic state

(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom (initialize-state))})

(defn apply-to-stateful-state! [id function & arguments]
  (swap! state-atom
         (fn [state]
           (apply apply-to-stateful-state
                  state id function arguments))))

(defn stateful-state! [id]
  (stateful-state @state-atom id))

(defn call-with-state! [id arguments initialize-state create-scene-graph]
  (apply call-with-state
         state-atom initialize-state create-scene-graph id arguments))



(ns flow-gl.gui.stateful)


(defn initialize-state []
  {})

(defn set-stateful-state [state id stateful-state]
  (assoc-in state
            [id]
            stateful-state))

(defn apply-to-stateful-state [state id function & arguments]
  (update-in state
             [id]
             (fn [stateful-state]
               (apply function
                      stateful-state
                      arguments))))

(defn stateful-state [state id]
  (get state id))

(defn call-with-state [state-atom initialize-state function id & arguments]
  (when (not (contains? @state-atom id))
    (swap! state-atom
           assoc id (initialize-state)))

  (apply function
         (get @state-atom id) id arguments))


(defn call-with-state-atom [state-atom id initialize-state function & arguments]
  (when (not (contains? @state-atom id))
    (swap! state-atom
           assoc id (initialize-state)))
  (let [stateful-state-atom (atom (get @state-atom id))
        result (apply function
                      stateful-state-atom
                      arguments)]
    
    (swap! state-atom
           set-stateful-state
           id
           @stateful-state-atom)
    result))

(defn delete-state [state])

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

(defn call-with-state! [id arguments initialize-state function]
  (apply call-with-state
         state-atom initialize-state function id arguments))

(defn call-with-state-atom! [id initialize-state function & arguments]
  (apply call-with-state-atom
         state-atom id initialize-state function arguments))



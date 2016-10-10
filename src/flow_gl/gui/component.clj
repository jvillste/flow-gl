(ns flow-gl.gui.component)


(defn initialize-state []
  (atom {}))

(defn apply-to-component-state [state id function & arguments]
  (update-in state
             [id]
             (fn [component-state]
               (apply function
                      component-state
                      arguments))))

(defn component-state [state id]
  (get state id))

(defn call [state-atom component id & arguments]
  (when (not (contains? @state-atom id))
    (swap! state-atom
           assoc id ((:initialize-state component))))

  (apply (:create-scene-graph component)
         (get @state-atom id) id arguments))

;; dynamic state

(def ^:dynamic state-atom)

(defn apply-to-component-state! [id function & arguments]
  (swap! state-atom
         (fn [state]
           (apply apply-to-component-state
                  state id function arguments))))


(defn component-state! [id]
  (get @state-atom id))

(defn call! [component id & arguments]
  (apply call
         state-atom component id arguments))



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


;; dynamic state

(def ^:dynamic state-atom)

(defn apply-to-component-state! [id function & arguments]
  (swap! state-atom
         (fn [state]
           (apply apply-to-component-state
                  state id function arguments))))


(defn component-state! [id]
  (get @state-atom id))



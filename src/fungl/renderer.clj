(ns fungl.renderer
  (:require (flow-gl.gui [scene-graph :as scene-graph]
                         [stateful :as stateful])))

(defn apply-renderers! [scene-graph gl]
  (scene-graph/update-depth-first scene-graph :render
                                  (fn [scene-graph]
                                    ((:render scene-graph) scene-graph gl)
                                    
                                    #_(reduce (fn [scene-graph renderer]
                                              (assert (:id renderer) "renderers must have an id")
                                              (stateful/call-with-state-atom! [::renderer (:id renderer)] 
                                                                              (or  (fn [] ((:initialize-state renderer) gl))
                                                                                   (constantly {}))
                                                                              (fn [state]
                                                                                (when-let [delete-state (:delete-state renderer)]
                                                                                  (delete-state state gl)))
                                                                              (:render renderer)
                                                                              gl
                                                                              scene-graph))
                                            scene-graph
                                            (:renderers scene-graph)))))


#_(defn initialize-state []
    {:renderer-states-atom (atom (stateful/initialize-state))})

#_(defn delete-state [state]
    (stateful/delete-state (:renderer-states-atom state)))

;; dynamic state


#_(def ^:dynamic state-atom)

#_(defn state-bindings []
    {#'state-atom (atom (initialize-state))})

#_(defn swap-state! [function & arguments]
    (apply swap! state-atom function arguments))

#_(defn apply-renderers! [scene-graph gl]
    (swap-state! apply-renderers scene-graph gl))



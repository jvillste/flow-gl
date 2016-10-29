(ns fungl.renderer
  (:require (flow-gl.gui [scene-graph :as scene-graph]
                         [stateful :as stateful])))

(defn apply-renderers [state scene-graph gl]
  (scene-graph/update-depth-first scene-graph :renderers
                                  (fn [scene-graph]
                                    
                                    (reduce (fn [scene-graph renderer]
                                              (assert (:id renderer) "renderers must have an id")
                                              (apply stateful/call-with-state-atom

                                                     (:renderer-states-atom state) 

                                                     (or  (fn [] ((:initialize-state renderer) gl))
                                                          (constantly {}))

                                                     (:render renderer)
                                                     
                                                     (:id renderer)
                                                     
                                                     [gl scene-graph]))
                                            scene-graph
                                            (:renderers scene-graph))))
  state)


(defn initialize-state []
  {:renderer-states-atom (atom (stateful/initialize-state))})

(defn delete-state [state]
  (stateful/delete-state (:renderer-states-atom state)))

;; dynamic state


(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom (initialize-state))})

(defn swap-state! [function & arguments]
  (apply swap! state-atom function arguments))

(defn apply-renderers! [scene-graph gl]
  (swap-state! apply-renderers scene-graph gl))



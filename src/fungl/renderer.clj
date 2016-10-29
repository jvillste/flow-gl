(ns fungl.renderer
  (:require (flow-gl.gui [scene-graph :as scene-graph]
                         [stateful :as stateful])))

(defn apply-renderers [state scene-graph gl]
  (scene-graph/update-depth-first scene-graph :renderers
                                  (fn [scene-graph]
                                    (assert (:id scene-graph) "scene graph with renderers must have an id")
                                    (reduce (fn [scene-graph renderer]
                                              (apply stateful/call-with-state-atom

                                                     (:renderer-states-atom state) 

                                                     (or  (fn [] ((:initialize-state renderer) gl))
                                                          (constantly {}))

                                                     (:render renderer)
                                                     
                                                     (:id renderer)
                                                     
                                                     [gl scene-graph]))
                                            scene-graph
                                            (:renderers scene-graph)))))


(defn initialize-state []
  {:renderer-states-atom (atom (stateful/initialize-state))})

(defn delete-state [state]
  (stateful/delete-state (:renderer-states-atom state)))

;; dynamic state


(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom (initialize-state))})

(defn apply-renderers! [scene-graph gl]
  (apply-renderers @state-atom
                   scene-graph
                   gl))



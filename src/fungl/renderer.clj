(ns fungl.renderer
  (:require (fungl [callable :as callable])
            (flow-gl.gui [scene-graph :as scene-graph]
                         [stateful :as stateful])))

(defn apply-renderers! [scene-graph gl]
  (scene-graph/update-depth-first scene-graph :render
                                  (fn [scene-graph]
                                    (callable/call (:render scene-graph)
                                                   scene-graph
                                                   gl))))

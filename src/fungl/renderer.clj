(ns fungl.renderer
  (:require [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.callable :as callable]))

(defn apply-renderers! [scene-graph gl]
  (scene-graph/update-depth-first scene-graph :render
                                  (fn [scene-graph]
                                    (callable/call (:render scene-graph)
                                                   scene-graph
                                                   gl))))

(ns fungl.renderer
  (:require [fungl.callable :as callable]))

(defn apply-renderers! [scene-graph gl]
  (let [scene-graph (if (and (:children scene-graph)
                             (not (:render-on-descend? scene-graph)))
                      (update-in scene-graph
                                 [:children]
                                 (fn [children]
                                   (doall (map (fn [child]
                                                 (apply-renderers! child gl))
                                               children))))
                      scene-graph)]

    (if (:render scene-graph)
      (let [result (callable/call (:render scene-graph)
                                  gl
                                  scene-graph)]
        result)
      scene-graph)))

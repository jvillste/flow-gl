(ns fungl.renderer
  (:require [fungl.callable :as callable]))

(defn apply-renderers! [gl layout-node]
  (let [layout-node (if (and (:children (:node layout-node))
                             (not (:render-on-descend? (:node layout-node))))
                      (update-in layout-node
                                 [:node :children]
                                 (fn [children]
                                   (doall (map (fn [child]
                                                 (apply-renderers! gl child))
                                               children))))
                      layout-node)]

    (if (:render (:node layout-node))
      (callable/call (:render (:node layout-node))
                     gl
                     layout-node)
      layout-node)))

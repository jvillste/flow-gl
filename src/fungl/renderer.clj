(ns fungl.renderer
  (:require [fungl.callable :as callable]
            [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(def ^:dynamic cache-atom)

(defn state-bindings []
  {#'cache-atom (hierarchical-identity-cache/create-cache-atom "apply-renderers!")})

(defn- apply-renderers!* [gl layout-node]
  (let [layout-node (if (and (:children (:node layout-node))
                             (not (:render-on-descend? (:node layout-node))))
                      (update-in layout-node
                                 [:node :children]
                                 (fn [children]
                                   (doall (map (fn [child]
                                                 (hierarchical-identity-cache/call-with-cache cache-atom
                                                                                              (:compilation-path (:node layout-node))
                                                                                              -1
                                                                                              apply-renderers!*
                                                                                              gl
                                                                                              child))
                                               children))))
                      layout-node)]

    (if (:render (:node layout-node))
      (callable/call (:render (:node layout-node))
                     gl
                     layout-node)
      layout-node)))

(defn apply-renderers! [gl layout-node]
  (hierarchical-identity-cache/with-cache-cleanup cache-atom
    (hierarchical-identity-cache/call-with-cache cache-atom
                                                 (:compilation-path (:node layout-node))
                                                 -1
                                                 apply-renderers!*
                                                 gl
                                                 layout-node)))

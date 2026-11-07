(ns fungl.renderer
  (:require [fungl.callable :as callable]
            [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(def ^:dynamic cache-atom)

(defn state-bindings []
  {#'cache-atom (hierarchical-identity-cache/create-cache-atom "apply-renderers!")})

(defn- apply-renderers!* [gl node]
  (let [node (if (and (:children node)
                      (not (:render-on-descend? node)))
               (update node
                       :children
                       (fn [children]
                         (doall (map (fn [child]
                                       (hierarchical-identity-cache/call-with-cache cache-atom
                                                                                    (:compilation-path node)
                                                                                    -1
                                                                                    apply-renderers!*
                                                                                    gl
                                                                                    child))
                                     children))))
               node)]
    (if (:render node)
      (callable/call (:render node)
                     gl
                     node)
      node)))

(defn apply-renderers! [gl layout-node]
  (hierarchical-identity-cache/with-cache-cleanup cache-atom
    (hierarchical-identity-cache/call-with-cache cache-atom
                                                 (:compilation-path layout-node)
                                                 -1
                                                 apply-renderers!*
                                                 gl
                                                 layout-node)))

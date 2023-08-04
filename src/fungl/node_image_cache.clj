(ns fungl.node-image-cache
  (:require [fungl.cache :as cache]
            [flow-gl.gui.scene-graph :as scene-graph]
            [flow-gl.gui.visuals :as visuals]))

(def ^:dynamic image-cache)

(defn state-bindings []
  {#'image-cache (cache/create-state 100)})

(defn apply-image-cache [scene-graph]
  (scene-graph/map-nodes (fn [node]
                           (if (cache/cached-2? image-cache
                                                visuals/render-to-images
                                                node)
                             (cache/call-with-cache image-cache
                                                    visuals/render-to-images
                                                    node)
                             node))
                         scene-graph
                         {:descend? (fn [node]
                                      (not (cache/cached-2? image-cache
                                                            visuals/render-to-images
                                                            node)))}))

(defn render-recurring-nodes-to-images [previous-scene-graph scene-graph]
  ;; (prn ) ;; TODO: remove me
  ;; (prn 'render-recurring-nodes-to-images) ;; TODO: remove me

  (let [previous-nodes (into #{} (scene-graph/enumerate-nodes previous-scene-graph))]
    (scene-graph/map-nodes (fn [node]
                             (if (contains? previous-nodes
                                            node)
                               (cache/call-with-cache image-cache
                                                      visuals/render-to-images
                                                      (apply-image-cache node))
                               node))
                           scene-graph
                           {:descend? (fn [node]
                                        (not (contains? previous-nodes
                                                        node)))})))

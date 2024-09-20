(ns fungl.node-image-cache
  (:require
   [flow-gl.gui.scene-graph :as scene-graph]
   [flow-gl.gui.visuals :as visuals]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(def ^:dynamic image-cache-atom)

(defn state-bindings []
  {#'image-cache-atom (hierarchical-identity-cache/create-cache-atom)})

(defn apply-image-cache [scene-graph]
  (scene-graph/map-nodes (fn [node]
                           (if (hierarchical-identity-cache/cached-call? image-cache-atom
                                                                         (:id node)
                                                                         1
                                                                         visuals/render-to-images
                                                                         node)
                             (hierarchical-identity-cache/call-with-cache image-cache-atom
                                                                          (:id node)
                                                                          1
                                                                          visuals/render-to-images
                                                                          node)
                             node))
                         scene-graph
                         {:descend? (fn [node]
                                      (not (hierarchical-identity-cache/cached-call? image-cache-atom
                                                                                     (:id node)
                                                                                     1
                                                                                     visuals/render-to-images
                                                                                     node)))}))

(defn apply-cache-and-render-to-images [scene-graph]
  (-> scene-graph
      (apply-image-cache)
      (visuals/render-to-images)))

(defn render-recurring-nodes-to-images [previous-scene-graph scene-graph]
  (if (nil? previous-scene-graph)
    scene-graph
    (hierarchical-identity-cache/with-cache-cleanup image-cache-atom
      (if (identical? previous-scene-graph scene-graph)
        (hierarchical-identity-cache/call-with-cache image-cache-atom
                                                     (:id scene-graph)
                                                     1
                                                     apply-cache-and-render-to-images
                                                     scene-graph)

        (if (contains? scene-graph :children)
          (update scene-graph
                  :children
                  (fn [children]
                    (mapv (fn [[previous-child child]]
                            (if (and (some? previous-child)
                                     (or (identical? previous-child child)
                                         (and (= (:type previous-child)
                                                 (:type child))
                                              (= (:local-id previous-child)
                                                 (:local-id child)))))
                              (render-recurring-nodes-to-images previous-child child)
                              child))
                          (partition 2 (interleave (concat (:children previous-scene-graph)
                                                           (repeat (max 0
                                                                        (- (count children)
                                                                           (count (:children previous-scene-graph))))
                                                                   nil))
                                                   children)))))
          scene-graph)))))

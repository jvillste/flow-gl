(ns fungl.node-image-cache
  (:require
   [flow-gl.gui.visuals :as visuals]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
   [fungl.layout :as layout]
   [fungl.log :as log]))

(def ^:dynamic image-cache-atom)

(defn state-bindings []
  {#'image-cache-atom (hierarchical-identity-cache/create-cache-atom "node-image-cache")})

(defn render-to-images [nodes-to-image-node node]
  (log/write "calling render-to-images" (:compilation-path node))
  (visuals/render-to-images nodes-to-image-node
                            (assoc node
                                   :x 0
                                   :y 0)))

(defn- render-recurring-nodes-to-images* [nodes-to-image-node previous-node node]
  (if (nil? previous-node)
    node
    (if (identical? (:unplaced-node previous-node)
                    (:unplaced-node node))
      (cond-> (hierarchical-identity-cache/call-with-cache-3 image-cache-atom
                                                             (:id node)
                                                             [(:unplaced-node node)]
                                                             []
                                                             render-to-images
                                                             nodes-to-image-node
                                                             node)
        (contains? node :x) (assoc :x (:x node))
        (contains? node :y) (assoc :y (:y node)))
      (if (some? (:children node))
        (update node
                :children
                (fn [children]
                  (mapv (fn [[previous-child child]]
                          (if (and (some? previous-child)
                                   (or (identical? previous-child child)
                                       (and (= (:type previous-child)
                                               (:type child))
                                            (= (:local-id previous-child)
                                               (:local-id child)))))
                            (render-recurring-nodes-to-images* nodes-to-image-node
                                                               previous-child
                                                               child)
                            child))
                        (partition 2
                                   (interleave (concat (:children previous-node)
                                                       (repeat (max 0
                                                                    (- (count children)
                                                                       (count (:children previous-node))))
                                                               nil))
                                               children)))))
        node))))

(defn render-recurring-nodes-to-images [nodes-to-image-node previous-scene-graph scene-graph]
  (log/write "render-recurring-nodes-to-images")
  ;; (prn "image-cache" (hierarchical-identity-cache/statistics image-cache-atom))
  #_scene-graph
  (hierarchical-identity-cache/with-cache-cleanup image-cache-atom
    (render-recurring-nodes-to-images* nodes-to-image-node
                                       previous-scene-graph
                                       scene-graph)))

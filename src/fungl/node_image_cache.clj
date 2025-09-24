(ns fungl.node-image-cache
  (:require
   [flow-gl.gui.visuals :as visuals]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
   [fungl.layout :as layout]
   [fungl.log :as log]))

(def ^:dynamic image-cache-atom)

(defn state-bindings []
  {#'image-cache-atom (hierarchical-identity-cache/create-cache-atom "node-image-cache")})

(defn render-to-images [nodes-to-image-node node layout]
  (visuals/render-to-images nodes-to-image-node
                            (assoc layout
                                   :node node)))

(defn apply-image-cache [nodes-to-image-node layout-node]
  (layout/map-layout-nodes (fn [layout-node]
                             (if (hierarchical-identity-cache/cached-call? image-cache-atom
                                                                           (-> layout-node :node :id)
                                                                           1
                                                                           render-to-images
                                                                           nodes-to-image-node
                                                                           (:node layout-node)
                                                                           (dissoc layout-node :node))
                               (do (log/write "using image from cache" (:compilation-path (:node layout-node)))
                                   (hierarchical-identity-cache/call-with-cache image-cache-atom
                                                                                (-> layout-node :node :id)
                                                                                1
                                                                                render-to-images
                                                                                nodes-to-image-node
                                                                                (:node layout-node)
                                                                                (dissoc layout-node :node)))
                               layout-node))
                           layout-node
                           {:descend? (fn [layout-node]
                                        (not (hierarchical-identity-cache/cached-call? image-cache-atom
                                                                                       (-> layout-node :node :id)
                                                                                       1
                                                                                       render-to-images
                                                                                       nodes-to-image-node
                                                                                       (:node layout-node)
                                                                                       (dissoc layout-node :node))))}))

(defn apply-cache-and-render-to-images [nodes-to-image-node node layout]
  (log/write "apply-cache-and-render-to-images" (:compilation-path node))
  (->> (assoc layout
              :node node)
       (apply-image-cache nodes-to-image-node)
       (visuals/render-to-images nodes-to-image-node)))

(defn- render-recurring-nodes-to-images* [nodes-to-image-node previous-layout-node layout-node]
  (if (nil? previous-layout-node)
    layout-node
    (if (and (identical? (:node previous-layout-node)
                         (:node layout-node))
             (= (dissoc previous-layout-node :node)
                (dissoc layout-node :node)))
      (hierarchical-identity-cache/call-with-cache image-cache-atom
                                                   (:id (:node layout-node))
                                                   1
                                                   apply-cache-and-render-to-images
                                                   nodes-to-image-node
                                                   (:node layout-node)
                                                   (dissoc layout-node :node))

      (if (some? (:children (:node layout-node)))
        (update-in layout-node
                   [:node :children]
                   (fn [children]
                     (mapv (fn [[previous-child-layout-node child-layout-node]]
                             (if (and (some? previous-child-layout-node)
                                      (or (identical? (:node previous-child-layout-node)
                                                      (:node child-layout-node))
                                          (and (= (:type (:node previous-child-layout-node))
                                                  (:type (:node child-layout-node)))
                                               (= (:local-id (:node previous-child-layout-node))
                                                  (:local-id (:node child-layout-node))))))
                               (render-recurring-nodes-to-images* nodes-to-image-node
                                                                  previous-child-layout-node
                                                                  child-layout-node)
                               child-layout-node))
                           (partition 2 (interleave (concat (:children (:node previous-layout-node))
                                                            (repeat (max 0
                                                                         (- (count children)
                                                                            (count (:children (:node previous-layout-node)))))
                                                                    nil))
                                                    children)))))
        layout-node))))

(defn render-recurring-nodes-to-images [nodes-to-image-node previous-scene-graph scene-graph]
  (log/write "render-recurring-nodes-to-images")
  ;; (prn "image-cache" (hierarchical-identity-cache/statistics image-cache-atom))
  (hierarchical-identity-cache/with-cache-cleanup image-cache-atom
    (render-recurring-nodes-to-images* nodes-to-image-node
                                       previous-scene-graph
                                       scene-graph)))

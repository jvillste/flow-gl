(ns fungl.node-image-cache
  (:require [fungl.cache :as cache]
            [flow-gl.gui.scene-graph :as scene-graph]
            [flow-gl.gui.visuals :as visuals]
            [medley.core :as medley]
            clojure.data))

(def ^:dynamic image-cache)

(defn state-bindings []
  {#'image-cache (cache/create-state 100)})

(defn apply-image-cache [scene-graph]
  (scene-graph/map-nodes (fn [node]
                           #_(prn 'cached?
                                  (cache/cached-2? image-cache
                                                   visuals/render-to-images
                                                   node)
                                  (:id node)) ;; TODO: remove me

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

(defn leaf-node-tree [scene-graph]
  (scene-graph/map-nodes (fn [node]
                           (if (and (:children node)
                                    (not (:render node)))
                             (select-keys node [:x :y :z :width :height :type :id :local-id])
                             node))
                         scene-graph))

(defn render-recurring-nodes-to-images [previous-scene-graph scene-graph]
  ;; (prn)
  ;; (prn 'render-recurring-nodes-to-images)

  (let [previous-leaf-node-tree (leaf-node-tree previous-scene-graph)
        current-leaf-node-tree (leaf-node-tree scene-graph)
        previous-nodes (into #{} (scene-graph/enumerate-nodes previous-leaf-node-tree))]
    (scene-graph/map-nodes (fn [node]
                             ;; (when-let [previous-node (scene-graph/get-in-path previous-leaf-node-tree
                             ;;                                                   (scene-graph/id-to-local-id-path (:id node)))]
                             ;;   (let [new-node (scene-graph/get-in-path current-leaf-node-tree
                             ;;                                           (scene-graph/id-to-local-id-path (:id node)))
                             ;;         diff (clojure.data/diff (dissoc previous-node :children)
                             ;;                                 (dissoc new-node :children))]
                             ;;     (when (not (nil? (first diff)))
                             ;;       (println)
                             ;;       (println "node changed: "
                             ;;                (:type node)
                             ;;                (:id node))
                             ;;       (println (second diff)))))

                             (if (contains? previous-nodes
                                            node)
                               (let [node (apply-image-cache node)]
                                 (if (= :rendered-to-images (:type node))
                                   node
                                   (cache/call-with-cache image-cache
                                                          visuals/render-to-images
                                                          node)))
                               node))
                           current-leaf-node-tree
                           {:descend? (fn [node]
                                        (not (contains? previous-nodes
                                                        node)))})))

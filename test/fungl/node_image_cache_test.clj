(ns fungl.node-image-cache-test
  (:require [clojure.test :refer [deftest is]]
            [flow-gl.gui.scene-graph :as scene-graph]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.dependable-atom :as dependable-atom]
            [fungl.layout :as layout]
            [fungl.layouts :as layouts]
            [fungl.node-image-cache :as node-image-cache]
            [fungl.view-compiler :as view-compiler]))

(deftest test-render-recurring-nodes-to-images
  (with-bindings (application/bindings)
    (let [height-atom (dependable-atom/atom 10)
          value-atom (dependable-atom/atom 0)
          static-view (fn []
                        {:type ::static
                         :width 10
                         :height 10})
          changing-view (fn []
                          {:type ::changing
                           :value @value-atom
                           :get-size (constantly {:width 10
                                                  :height @height-atom})})
          root-view (fn []
                      (layouts/vertically [changing-view]
                                          [static-view]))
          compile-and-layout-scene-graph (fn []
                                           (-> (view-compiler/compile-view-calls [root-view])
                                               (layout/layout-scene-graph 100
                                                                          100)))
          first-scene-graph (compile-and-layout-scene-graph)
          _ (swap! value-atom inc)

          second-scene-graph (compile-and-layout-scene-graph)
          nodes-to-image-node (constantly nil)
          render-count-atom (atom 0)
          render-to-image (fn [_nodes-to-image-node layout-node]
                            (is (= 0 (:x layout-node)))
                            (is (= 0 (:y layout-node)))
                            (swap! render-count-atom inc)
                            (assoc layout-node
                                   :node {:type ::rendered-to-image}))]

      (is (= '{:type :fungl.layouts/vertical-stack,
               :x 0,
               :y 0,
               :width 100,
               :height 100,
               :children
               ({:type :fungl.node-image-cache-test/changing,
                 :x 0,
                 :y 0,
                 :width 10,
                 :height 10}
                {:type :fungl.node-image-cache-test/static,
                 :x 0,
                 :y 10,
                 :width 10,
                 :height 10})} (scene-graph/select-node-keys [:type :x :y :width :height] first-scene-graph)))

      (is (not (identical? (first (:children first-scene-graph))
                           (first (:children second-scene-graph)))))
      (is (identical? (second (:children first-scene-graph))
                      (second (:children second-scene-graph))))

      (with-redefs [visuals/render-to-images render-to-image]
        (is (= '{:type :fungl.layouts/vertical-stack,
                 :x 0,
                 :y 0,
                 :width 100,
                 :height 100,
                 :children
                 ({:type :fungl.node-image-cache-test/changing,
                   :x 0,
                   :y 0,
                   :width 10,
                   :height 10}
                  {:type :fungl.node-image-cache-test/static,
                   :x 0,
                   :y 10,
                   :width 10,
                   :height 10,
                   :node {:type :fungl.node-image-cache-test/rendered-to-image}})}
               (scene-graph/select-node-keys [:type :x :y :width :height]
                                             (node-image-cache/render-recurring-nodes-to-images nodes-to-image-node
                                                                                                first-scene-graph
                                                                                                second-scene-graph))))
        (is (= 1 @render-count-atom))
        (swap! height-atom inc)
        (let [moved-scene-graph (compile-and-layout-scene-graph)]
          (is (not (identical? (second (:children second-scene-graph))
                               (second (:children moved-scene-graph)))))
          (is (not= (:y (second (:children second-scene-graph)))
                    (:y (second (:children moved-scene-graph)))))
          (is (identical? (:unplaced-node (second (:children second-scene-graph)))
                          (:unplaced-node (second (:children moved-scene-graph)))))
          (is (= '{:type :fungl.layouts/vertical-stack,
                   :x 0,
                   :y 0,
                   :width 100,
                   :height 100,
                   :children
                   ({:type :fungl.node-image-cache-test/changing,
                     :x 0,
                     :y 0,
                     :width 10,
                     :height 10}
                    {:type :fungl.node-image-cache-test/static,
                     :x 0,
                     :y 10,
                     :width 10,
                     :height 10,
                     :node {:type :fungl.node-image-cache-test/rendered-to-image}})}
                 (scene-graph/select-node-keys [:type :x :y :width :height]
                                               (node-image-cache/render-recurring-nodes-to-images nodes-to-image-node
                                                                                                  first-scene-graph
                                                                                                  second-scene-graph))))
          (is (= 1 @render-count-atom)))))))

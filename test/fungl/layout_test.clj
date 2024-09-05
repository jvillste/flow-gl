(ns fungl.layout-test
  (:require [fungl.layout :as layout]
            [fungl.layouts :as layouts]
            [clojure.test :refer [deftest is]]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.view-compiler :as view-compiler]
            [fungl.cache :as cache]))

(deftest test-adapt-to-space
  (is (= '{:type :fungl.layouts/vertical-stack,
           :local-id :new-root,
           :id [],
           :width 100,
           :height 2147483747,
           :available-width 100,
           :available-height 100,
           :children
           ({:local-id :a,
             :id [:a],
             :x 0,
             :y 0,
             :width 100,
             :height 2147483647,
             :available-width 100,
             :available-height 2147483647}
            {:local-id :b,
             :id [:b],
             :x 0,
             :y 2147483647,
             :width 100,
             :height 100,
             :available-width 100,
             :available-height 2147483647})}
         (with-bindings (view-compiler/state-bindings)
           (layout/select-layout-keys (layout/layout-scene-graph (view-compiler/compile-view-calls false
                                                                                                   []
                                                                                                   {:local-id :a
                                                                                                    :adapt-to-space (fn [node]
                                                                                                                      (-> (layouts/vertically-2 {}
                                                                                                                                                (dissoc node :adapt-to-space)
                                                                                                                                                {:width 100 :height 100 :local-id :b})
                                                                                                                          (assoc :local-id :new-root
                                                                                                                                 :available-width (:available-width node)
                                                                                                                                 :available-height (:available-height node))))})
                                                                 100 100))))))

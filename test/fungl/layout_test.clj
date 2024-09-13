(ns fungl.layout-test
  (:require [fungl.layout :as layout]
            [fungl.layouts :as layouts]
            [clojure.test :refer [deftest is]]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.view-compiler :as view-compiler]
            [fungl.cache :as cache]
            [fungl.component.text-area :as text-area]
            [fungl.application :as application]
            [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(deftest test-adapt-to-space
  (is (= '{:type :fungl.layouts/vertical-stack,
           :local-id :new-root,
           :id [],
           :x 0,
           :y 0,
           :width 100,
           :height 100,
           :children
           ({:local-id :a,
             :id [:a],
             :x 0,
             :y 0,
             :width 100,
             :height 100,
             :children nil}
            {:local-id :b,
             :id [:b],
             :x 0,
             :y 100,
             :width 100,
             :height 100,
             :children nil})}
         (with-bindings (merge (view-compiler/state-bindings)
                               (layout/state-bindings))
           (layout/select-layout-keys (layout/layout-scene-graph (view-compiler/compile-view-calls {:local-id :a
                                                                                                    :adapt-to-space (fn [node available-width available-height]
                                                                                                                      {:node (layouts/vertically-2 {}
                                                                                                                                                   {:width 100 :height 100 :local-id :a}
                                                                                                                                                   {:width 100 :height 100 :local-id :b})
                                                                                                                       :local-id :new-root})})
                                                                 100 100))))))

(defn child [label]
  (text-area/text label))

(defn child-list [count]
  (layouts/vertically-2 {}
                        (text-area/text (str "the count is " count))
                        (for [index (range count)]
                          [child (str "child " index)])))

(defn compile-view-calls [view-call]
  (view-compiler/start-compilation-cycle!)
  (let [scene-graph (view-compiler/compile-view-calls view-call)]
    (view-compiler/end-compilation-cycle!)
    scene-graph))

(deftest test-layout-cache
  (with-bindings (merge (application/create-event-handling-state)
                        (application/create-render-state))

    (is (identical? (compile-view-calls [child-list 2])
                    (compile-view-calls [child-list 2])))

    (is (not (identical? (compile-view-calls [child-list 2])
                         (compile-view-calls [child-list 1]))))

    (is (not (identical? (first (:children (compile-view-calls [child-list 2])))
                         (first (:children (compile-view-calls [child-list 1]))))))

    (is (identical? (nth (:children (compile-view-calls [child-list 2]))
                         1)
                    (nth (:children (compile-view-calls [child-list 1]))
                         1)))

    (let [scene-graph (compile-view-calls [child-list 2])]
      (layout/layout-scene-graph scene-graph
                                 100 100)
      (is (= {:miss-count 6, :mapping-count 6}
             (hierarchical-identity-cache/statistics layout/layout-cache-atom)))
      ;;      (def cache @layout/layout-cache-atom)
      (layout/layout-scene-graph scene-graph
                                 100 100)
      (is (= {:hit-count 3, :mapping-count 6}
             (hierarchical-identity-cache/statistics layout/layout-cache-atom))))))

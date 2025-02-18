(ns fungl.layout-test
  (:require [fungl.layout :as layout]
            [fungl.layouts :as layouts]
            [clojure.test :refer [deftest is]]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.view-compiler :as view-compiler]
            [fungl.cache :as cache]
            [fungl.component.text-area :as text-area]
            [fungl.application :as application]
            [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
            [fungl.node-image-cache :as node-image-cache]
            [flow-gl.graphics.font :as font]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.string :as string]
            [flow-gl.gui.visuals :as visuals]
            [clojure.core.async :as async]
            [clj-async-profiler.core :as clj-async-profiler]))

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
           (layout/select-layout-keys (layout/apply-layout-nodes (layout/layout-scene-graph (view-compiler/compile-view-calls {:local-id :a
                                                                                                                               :adapt-to-space (fn [_node _available-width _available-height]
                                                                                                                                                 {:node (layouts/vertically-2 {}
                                                                                                                                                                              {:width 100 :height 100 :local-id :a}
                                                                                                                                                                              {:width 100 :height 100 :local-id :b})
                                                                                                                                                  :local-id :new-root})})
                                                                                            100 100)))))))


(def font (font/create-by-name "CourierNewPSMT" 40))

(defn text [string]
  (text-area/text (str string)
                  [200 200 200 255]
                  font))

(defn child [label]
  (text label))

(defn child-list [count]
  (layouts/vertically-2 {}
                        (text (str "the count is " count))
                        (for [index (range count)]
                          [child (str "child " index)])))

(defn compile-view-calls [view-call]
  (view-compiler/start-compilation-cycle!)
  (let [scene-graph (view-compiler/compile-view-calls view-call)]
    (view-compiler/end-compilation-cycle!)
    scene-graph))

(defn render-scene-graphs [& view-calls]
  (loop [view-calls view-calls
         previous-scene-graph nil
         previous-image nil]

    (if-some [view-call (first view-calls)]
      (let [scene-graph (layout/layout-scene-graph (compile-view-calls view-call)
                                                   100 100)
            image (node-image-cache/render-recurring-nodes-to-images previous-scene-graph
                                                                     scene-graph)]
        (prn view-call)
        (prn "image-cache-atom"
             (hierarchical-identity-cache/statistics node-image-cache/image-cache-atom)
             (:id scene-graph))
        (prn "scene graph"
             (identical? previous-scene-graph scene-graph)
             (scene-graph/select-node-keys [:type] (layout/apply-layout-nodes scene-graph)))
        (prn "image"
             (identical? previous-image image)
             (scene-graph/select-node-keys [:type] (layout/apply-layout-nodes image)))
        (recur (rest view-calls)
               scene-graph
               image))
      previous-image)))

(comment
  (with-bindings (merge (application/create-event-handling-state)
                        (application/create-render-state)
                        {#'hierarchical-identity-cache/maximum-number-of-cycles-without-removing-unused-keys 0})
    (scene-graph/select-node-keys [:id :type]
                                  (layout/layout-scene-graph (compile-view-calls [child-list 2])
                                                             100 100)))

  ;; => {:id [],
  ;;     :type :fungl.layouts/vertical-stack,
  ;;     :children
  ;;     ({:id [0], :type :flow-gl.gui.visuals/text-area}
  ;;      {:id [1], :type :flow-gl.gui.visuals/text-area}
  ;;      {:id [2], :type :flow-gl.gui.visuals/text-area})}

  ) ;; TODO: remove me


(deftest test-layout-cache
  (with-bindings (merge (application/create-event-handling-state)
                        (application/create-render-state)
                        {#'hierarchical-identity-cache/maximum-number-of-cycles-without-removing-unused-keys 0})

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


    (is (identical? (layout/layout-scene-graph (compile-view-calls [child-list 2])
                                               100 100)
                    (layout/layout-scene-graph (compile-view-calls [child-list 2])
                                               100 100)))

    (is (not (identical? (layout/layout-scene-graph (compile-view-calls [child-list 2])
                                                    100 100)
                         (layout/layout-scene-graph (compile-view-calls [child-list 1])
                                                    100 100))))

    (is (not (identical? (:node (first (:children (:node (layout/layout-scene-graph (compile-view-calls [child-list 2])
                                                                                    100 100)))))
                         (:node (first (:children (:node (layout/layout-scene-graph (compile-view-calls [child-list 1])
                                                                                    100 100))))))))


    (let [child-1 (nth (:children (layout/layout-scene-graph (compile-view-calls [child-list 2])
                                                             100 100))
                       1)
          child-2 (nth (:children (layout/layout-scene-graph (compile-view-calls [child-list 1])
                                                             100 100))
                       1)]
      (is (identical? (:node child-1) (:node child-2))))


    (let [scene-graph (compile-view-calls [child-list 2])]
      (layout/layout-scene-graph scene-graph
                                 100 100)
      (is (= {:miss-count 3, :hit-count 1, :mapping-count 4}
             (hierarchical-identity-cache/statistics layout/layout-node-cache-atom)))
      (layout/layout-scene-graph scene-graph
                                 100 100)
      (is (= {:hit-count 1, :mapping-count 4}
             (hierarchical-identity-cache/statistics layout/layout-node-cache-atom)))

      (is (identical? (layout/layout-scene-graph scene-graph
                                                 100 100)
                      (layout/layout-scene-graph scene-graph
                                                 100 100))))



    (let [scene-graph (layout/layout-scene-graph (compile-view-calls [child-list 2])
                                                 100 100)
          image-1 (node-image-cache/render-recurring-nodes-to-images scene-graph
                                                                     scene-graph)
          _ (prn "image-cache-atom"
                 (hierarchical-identity-cache/statistics node-image-cache/image-cache-atom)
                 (:id scene-graph))

          image-2 (node-image-cache/render-recurring-nodes-to-images scene-graph
                                                                     scene-graph)
          _ (prn "image-cache-atom"
                 (hierarchical-identity-cache/statistics node-image-cache/image-cache-atom)
                 (:id scene-graph))]

      (identical? image-1 image-2)

      (let [scene-graph-2 (layout/layout-scene-graph (compile-view-calls [child-list 3])
                                                     100 100)
            image-3 (node-image-cache/render-recurring-nodes-to-images scene-graph
                                                                       scene-graph-2)
            _ (prn "image-cache-atom"
                   (hierarchical-identity-cache/statistics node-image-cache/image-cache-atom)
                   (:id scene-graph))

            scene-graph-3 (layout/layout-scene-graph (compile-view-calls [child-list 4])
                                                     100 100)

            image-3 (node-image-cache/render-recurring-nodes-to-images scene-graph-2
                                                                       scene-graph-3)]

        (prn (scene-graph/select-node-keys [:type] scene-graph))
        (prn (scene-graph/select-node-keys [:type] image-1))
        (prn (scene-graph/select-node-keys [:type] scene-graph-2))
        (prn (scene-graph/select-node-keys [:type] image-3))))


    (let [first-view-call [child-list 1]]
      (render-scene-graphs first-view-call
                           first-view-call
                           first-view-call
                           [child-list 2]
                           [child-list 3]
                           ))

    (render-scene-graphs [child-list 1]
                         [child-list 2])
    (is (= '{:type :fungl.layouts/vertical-stack,
             :children
             ({:type :flow-gl.gui.visuals/text-area}
              {:type :rendered-to-images, :children ({:type :image})}
              {:type :flow-gl.gui.visuals/text-area})}
           (scene-graph/select-node-keys [:type] (layout/apply-layout-nodes (render-scene-graphs [child-list 1]
                                                                                                 [child-list 2])))))
    )

  )


(comment
  (test-layout-cache)
  ) ;; TODO: remove me


(defn random-text-editor []
  (let [state-atom (dependable-atom/atom "random-text-editor-state" (string/trim (apply str (repeatedly 20 #(rand-nth "      abcdefghijklmnopqrstuvwxyz")))))]
    (fn []
      [text-area/text-area-3 {:style {:color [255 0 0 255]
                                      :font  font}
                              :text @state-atom
                              :on-text-change (fn [new-value]
                                                (reset! state-atom new-value))}])))

(defn stateless-component []
  (text "foo"))

(defn constructor-cache-test-root []
  (let [state-atom (dependable-atom/atom 2)]
    (fn []
      (assoc (layouts/vertically-2 {}
                                   (repeat @state-atom [random-text-editor]))
             :keyboard-event-handler (fn [_scene-graph event]
                                       (when (and (= :descent (:phase event))
                                                  (= :key-pressed (:type event)))
                                         (when (= (:key event) :n)
                                           (swap! state-atom inc))
                                         (when (= (:key event) :p)
                                           (swap! state-atom dec)))
                                       event)))))

(defn performance-test-root []
  (layouts/vertically-2 {}
                        (map vector (repeat 50 random-text-editor))))

(defn image-cache-test-root []
  (layouts/vertically-2 {}
                        {:view-call [random-text-editor]
                         :local-id :editor-1}

                        {:local-id :clip
                         :node (visuals/clip {:local-id :vertically
                                              :node (layouts/vertically-2 {}
                                                                          {:view-call [random-text-editor]
                                                                           :local-id :editor-2}
                                                                          {:local-id :vertically-2
                                                                           :node (layouts/vertically-2 {}
                                                                                                       {:view-call [random-text-editor]
                                                                                                        :local-id :editor-3}
                                                                                                       {:view-call [random-text-editor]
                                                                                                        :local-id :editor-4})})})}))


;; TODO: image cache gets only one image when it should get 99 images, one for each unchanged text, with-minimun-size and superimpose need to be ported to layout nodes
(comment
  (application/start-application (fn [] [random-text-editor]))



  (application/start-application (fn [] [image-cache-test-root]))


  (application/start-application (fn [] [performance-test-root]))

  (with-bindings (merge (application/create-event-handling-state)
                        (application/create-render-state)
                        {#'hierarchical-identity-cache/maximum-number-of-cycles-without-removing-unused-keys 0})

    [(layout/select-layout-node-keys [] [:type] (layout/layout-scene-graph (compile-view-calls [performance-test-root])
                                                                           100 100))
     (layout/layout-scene-graph (compile-view-calls [performance-test-root])
                                100 100)]
    #_(scene-graph/select-node-keys [:type] (layout/apply-layout-nodes (layout/layout-scene-graph (compile-view-calls [performance-test-root])
                                                                                                  100 100))))


  ) ;; TODO: remove me


(def key-press-events [{:key-code 83,
                        :alt? false,
                        :key :s,
                        :meta? false,
                        :control? false,
                        :time 1726892602125,
                        :type :key-pressed,
                        :source :keyboard,
                        :shift? false,
                        :is-auto-repeat nil,
                        :character \s}

                       {:key-code 0,
                        :alt? false,
                        :key :undefined,
                        :meta? false,
                        :control? false,
                        :time 1726892602125,
                        :type :key-typed,
                        :source :keyboard,
                        :shift? false,
                        :is-auto-repeat nil,
                        :character \s}
                       {:key-code 83,
                        :alt? false,
                        :key :s,
                        :meta? false,
                        :control? false,
                        :time 1726892602201,
                        :type :key-released,
                        :source :keyboard,
                        :shift? false,
                        :is-auto-repeat nil,
                        :character \s}
                       ])

(defn interval [framerate]
  (let [millisecond 1000000]
    (/ (* 1000 millisecond)
       framerate)))
(comment

  (clj-async-profiler/serve-ui 9898)

  (let [event-channel (application/start-application (fn [] [performance-test-root]))]

    ;;    (Thread/sleep 500)

    (clj-async-profiler/profile {:event :cpu #_:alloc
                                 :interval (interval 1000)}
                                (doseq [event (apply concat (repeat 100 key-press-events))]
                                  ;;      (Thread/sleep 10)
                                  (async/>!! event-channel
                                             event)))


    ;;    (Thread/sleep 500)

    (async/>!! event-channel
               {:type :close-requested}))


  ) ;; TODO: remove me



(deftest test-compilation-cache
  (with-bindings (merge (view-compiler/state-bindings)
                        (layout/state-bindings)
                        )
    (let [view (fn []
                 {:type :view})
          scene-graph (view-compiler/call-compile-node-with-cache [] [] [view])
          layout (layout/layout-scene-graph scene-graph 1 1)
          scene-graph-2 (view-compiler/call-compile-node-with-cache [] [] [view])
          layout-2 (layout/layout-scene-graph scene-graph 1 1)]
      (is (= {:type :view
              :id []
              :compilation-path [:view-call]}
             scene-graph))
      (is (identical? scene-graph
                      scene-graph-2))

      (is (identical? layout
                      layout-2)))))

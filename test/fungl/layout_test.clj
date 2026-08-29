(ns fungl.layout-test
  (:require
   [clojure.core.async :as async] ;;            [clj-async-profiler.core :as clj-async-profiler]
   [clojure.data :as data]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.scene-graph :as scene-graph]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.component.text-area :as text-area]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
   [fungl.layout :as layout]
   [fungl.layouts :as layouts]
   [fungl.node-image-cache :as node-image-cache]
   [fungl.util :as util :refer [remove-indentation]]
   [fungl.view-compiler :as view-compiler]))

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
                                                                                                    :adapt-to-space (fn [_node _available-width _available-height]
                                                                                                                      {:node (layouts/vertically-2 {}
                                                                                                                                                   {:width 100 :height 100 :local-id :a}
                                                                                                                                                   {:width 100 :height 100 :local-id :b})
                                                                                                                       :local-id :new-root})})
                                                                 100 100))))))


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
  (view-compiler/compile-view-calls view-call))

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
             (scene-graph/select-node-keys [:type] scene-graph))
        (prn "image"
             (identical? previous-image image)
             (scene-graph/select-node-keys [:type] image))
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
           (scene-graph/select-node-keys [:type] (render-scene-graphs [child-list 1]
                                                                      [child-list 2]))))
    ))

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

(deftest test-layout-scene-graph
  (with-bindings (application/bindings)
    (let [scene-graph (layouts/vertically-2 {:margin 50 :fill-width? true}
                                            (layouts/box 10
                                                         (visuals/rectangle-2)
                                                         (visuals/text-area "foo")
                                                         ;; {:fill-width? true}
                                                         )
                                            (layouts/box 10
                                                         (visuals/rectangle-2)
                                                         (visuals/text-area "foobarbaz")
                                                         ;; {:fill-width? true}
                                                         ))
          layouted-scene-graph (layout/layout-scene-graph (view-compiler/compile-view-calls scene-graph)
                                                          500
                                                          500)]

      (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type]
                                                                   layouted-scene-graph))
      ;; (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type] layouted-scene-graph))

      ;; (is (= (remove-indentation "{:type :fungl.layouts/vertical-stack}
      ;;                               {:type :fungl.layouts/box}
      ;;                                 {:type :flow-gl.gui.visuals/rectangle}
      ;;                                 {:type :flow-gl.gui.visuals/text-area}
      ;;                               {:type :fungl.layouts/box}
      ;;                                 {:type :flow-gl.gui.visuals/rectangle}
      ;;                                 {:type :flow-gl.gui.visuals/text-area}
      ;;                             ")
      ;;        (with-out-str (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type] scene-graph)))))

      ;; (is (= (remove-indentation "{:x 0, :y 0, :width 2147483647, :height 2147483647}
      ;;                               {:width 2147483647, :height 203.28125, :type :fungl.layouts/vertical-stack}
      ;;                                 {:x 0, :y 0, :width 2147483647, :height 76.640625}
      ;;                                   {:width 2147483647, :height 76.640625, :type :fungl.layouts/box}
      ;;                                     {:x 0, :y 0, :width 2147483647, :height 76.640625}
      ;;                                       {:width 2147483647, :height 2147483647, :type :flow-gl.gui.visuals/rectangle}
      ;;                                     {:x 10, :y 10, :width 90.0, :height 56.640625}
      ;;                                       {:width 90.0, :height 56.640625, :type :flow-gl.gui.visuals/text-area}
      ;;                                 {:x 0, :y 126.640625, :width 2147483647, :height 76.640625}
      ;;                                   {:width 2147483647, :height 76.640625, :type :fungl.layouts/box}
      ;;                                     {:x 0, :y 0, :width 2147483647, :height 76.640625}
      ;;                                       {:width 2147483647, :height 2147483647, :type :flow-gl.gui.visuals/rectangle}
      ;;                                     {:x 10, :y 10, :width 270.0, :height 56.640625}
      ;;                                       {:width 270.0, :height 56.640625, :type :flow-gl.gui.visuals/text-area}
      ;;                             ")
      ;;        (with-out-str (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type] layouted-scene-graph)))))

      ;; (is (= (remove-indentation "{:x 0, :y 0, :width 2147483647, :height 2147483647, :type :fungl.layouts/vertical-stack}
      ;;                               {:x 0, :y 0, :width 2147483647, :height 76.640625, :type :fungl.layouts/box}
      ;;                                 {:x 0, :y 0, :width 2147483647, :height 76.640625, :type :flow-gl.gui.visuals/rectangle}
      ;;                                 {:x 10, :y 10, :width 90.0, :height 56.640625, :type :flow-gl.gui.visuals/text-area}
      ;;                               {:x 0, :y 126.640625, :width 2147483647, :height 76.640625, :type :fungl.layouts/box}
      ;;                                 {:x 0, :y 0, :width 2147483647, :height 76.640625, :type :flow-gl.gui.visuals/rectangle}
      ;;                                 {:x 10, :y 10, :width 270.0, :height 56.640625, :type :flow-gl.gui.visuals/text-area}
      ;;                             ")
      ;;        (with-out-str (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type] (layout/apply-layout-nodes layouted-scene-graph))))))
      ))



  (testing "box :fill-width true no :set-available-width-to-fit-the-widest-child"
    (is (= (remove-indentation "{:x 0, :y 0, :width 500, :height 500, :type :fungl.layouts/vertical-stack}
                                  {:x 0, :y 0, :width 500, :height 40, :type :fungl.layouts/box}
                                    {:width 500, :height 2147483647, :type :rectangle}
                                    {:width 50, :height 20, :type :text}
                                  {:x 0, :y 90, :width 500, :height 40, :type :fungl.layouts/box}
                                    {:width 500, :height 2147483647, :type :rectangle}
                                    {:width 100, :height 20, :type :text}
                                ")
           (with-out-str (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type]
                                                                                      (with-bindings (application/bindings)
                                                                                        (layout/layout-scene-graph (layouts/vertically-2 {:margin 50}
                                                                                                                                         (layouts/box 10
                                                                                                                                                      {:type :rectangle}
                                                                                                                                                      {:type :text
                                                                                                                                                       :get-size (constantly {:width 50 :height 20})}
                                                                                                                                                      {:fill-width? true})
                                                                                                                                         (layouts/box 10
                                                                                                                                                      {:type :rectangle}
                                                                                                                                                      {:type :text
                                                                                                                                                       :get-size (constantly {:width 100 :height 20})}
                                                                                                                                                      {:fill-width? true}))
                                                                                                                   500
                                                                                                                   500)))))))))

(deftest test-vertical-stack
  (with-bindings (application/bindings)
    (is (= '{:children ({:height 50, :width 10, :x 0, :y 0}),
             :height 500,
             :type :fungl.layouts/vertical-stack,
             :width 500,
             :x 0,
             :y 0}
           (scene-graph/select-node-keys [:x :y :width :height :type]
                                         (-> (layouts/vertically-2 {:margin 50}
                                                                   {:width 10
                                                                    :height 50})
                                             (view-compiler/compile-view-calls)
                                             (layout/layout-scene-graph 500
                                                                        500)))))

    (is (= '{:children ({:height 50, :width 10, :x 0, :y 0}),
             :height 500,
             :type :fungl.layouts/vertical-stack,
             :width 500,
             :x 0,
             :y 0}
           (scene-graph/select-node-keys [:x :y :width :height :type]
                                         (-> (layouts/vertically-2 {:margin 50}
                                                                   {:get-size (fn [_node _available-width _available-height]
                                                                                {:width 10
                                                                                 :height 50})})
                                             (view-compiler/compile-view-calls)
                                             (layout/layout-scene-graph 500
                                                                        500)))))))

(deftest test-box-layout
  (with-bindings (application/bindings)
    (testing ":fill-width false"
      (is (= (remove-indentation "{:x 0, :y 0, :width 500, :height 500, :type :fungl.layouts/box}
                                    {:x 0, :y 0, :width 120, :height 40, :type :rectangle}
                                    {:x 10, :y 10, :width 100, :height 20, :type :text}
                                 ")
             (with-out-str (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type]
                                                                                        (layout/layout-scene-graph (layouts/box 10
                                                                                                                                {:type :rectangle}
                                                                                                                                {:type :text
                                                                                                                                 :get-size (constantly {:width 100 :height 20})}
                                                                                                                                {:fill-width? false})
                                                                                                                   500
                                                                                                                   500)))))))
    (testing ":fill-width true"
      (is (= (remove-indentation "{:x 0, :y 0, :width 500, :height 500, :type :fungl.layouts/box}
                                    {:x 0, :y 0, :width 500, :height 40, :type :rectangle}
                                    {:x 10, :y 10, :width 100, :height 20, :type :text}
                                 ")
             (with-out-str (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type]
                                                                                        (layout/layout-scene-graph (layouts/box 10
                                                                                                                                {:type :rectangle}
                                                                                                                                {:type :text
                                                                                                                                 :get-size (constantly {:width 100 :height 20})}
                                                                                                                                {:fill-width? true})
                                                                                                                   500
                                                                                                                   500)))))))))


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
  (with-bindings (application/bindings)
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

(defn scene-graph-to-string [scene-graph]
  (with-out-str (scene-graph/print-scene-graph (scene-graph/select-node-keys [:x :y :width :height :type :string]
                                                                             scene-graph))))

(deftest test-layout-cache-2
  (with-bindings (application/bindings)
    (let [original-vertical-stack-get-size @#'layouts/vertical-stack-get-size
          vertical-stack-get-size-call-count-atom (atom 0)]
      (with-redefs [layouts/vertical-stack-get-size (fn [& arguments]
                                                      (swap! vertical-stack-get-size-call-count-atom inc)
                                                      (apply original-vertical-stack-get-size arguments))]
        (let [count-atom (dependable-atom/atom 0)
              static-view (fn []
                            (visuals/text-area "foo"))

              counter-view (fn []
                             (println "counter in counter-view" @count-atom)
                             (visuals/text-area (str @count-atom)))

              root-view (fn [] (layouts/with-margin 10
                                 (layouts/vertically-2 {}
                                                       [static-view]
                                                       [counter-view])))
              root-view-call [root-view]

              scene-graph (view-compiler/compile-view-calls root-view-call)

              layout (layout/layout-scene-graph scene-graph
                                                500
                                                500)

              scene-graph-2 (view-compiler/compile-view-calls root-view-call)

              layout-2 (layout/layout-scene-graph scene-graph-2
                                                  500
                                                  500)
              ]

          (is (= 0 @vertical-stack-get-size-call-count-atom))
          (layout/layout-scene-graph scene-graph
                                     500
                                     500)

          (is (= (remove-indentation "{:type :fungl.layouts/with-margins}
                                        {:type :fungl.layouts/vertical-stack}
                                          {:type :flow-gl.gui.visuals/text-area, :string \"foo\"}
                                          {:type :flow-gl.gui.visuals/text-area, :string \"0\"}
                                      ")
                 (scene-graph-to-string scene-graph)))

          ;; (is (= (remove-indentation "{:x 0, :y 0, :width 500, :height 500}
          ;;                             {:width 90.0, :height 113.28125, :type :fungl.layouts/vertical-stack}
          ;;                               {:x 0, :y 0, :width 90.0, :height 56.640625}
          ;;                                 {:width 90.0, :height 56.640625, :type :flow-gl.gui.visuals/text-area, :string \"foo\"}
          ;;                               {:x 0, :y 56.640625, :width 90.0, :height 56.640625}
          ;;                                 {:width 30.0, :height 56.640625, :type :flow-gl.gui.visuals/text-area, :string \"0\"}
          ;;                           ")
          ;;        (scene-graph-to-string layout)))

          (is (identical? scene-graph
                          scene-graph-2))

          (is (identical? layout
                          layout-2))

          (println "incrementing counter")
          (swap! count-atom inc)

          ;; (println "ounter is now" @count-atom)

          (let [scene-graph-3 (view-compiler/compile-view-calls root-view-call)
                layout-3 (layout/layout-scene-graph scene-graph-3
                                                    500
                                                    500)]

            (is (= (remove-indentation "{:type :fungl.layouts/with-margins}
                                          {:type :fungl.layouts/vertical-stack}
                                            {:type :flow-gl.gui.visuals/text-area, :string \"foo\"}
                                            {:type :flow-gl.gui.visuals/text-area, :string \"1\"}
                                        ")
                   (scene-graph-to-string scene-graph-3)))

            (is (= (remove-indentation "{:x 0, :y 0, :width 500, :height 500, :type :fungl.layouts/with-margins}
                                          {:x 10, :y 10, :width 40.0, :height 68.994140625, :type :fungl.layouts/vertical-stack}
                                            {:x 0, :y 0, :width 40.0, :height 34.4970703125, :type :flow-gl.gui.visuals/text-area, :string \"foo\"}
                                            {:x 0, :y 34.4970703125, :width 40.0, :height 34.4970703125, :type :flow-gl.gui.visuals/text-area, :string \"1\"}
                                        ")
                   (scene-graph-to-string layout-3)))

            (is (identical? (-> scene-graph :children first :children first)
                            (-> scene-graph-2 :children first :children first)))

            (is (not (identical? (-> scene-graph :children first :children second)
                                 (-> scene-graph-3 :children first :children second))))

            (is (= (-> layout :children first :children first)
                   (-> layout-3 :children first :children first)))

            (is (identical? (-> layout :children first :children first)
                            (-> layout-3 :children first :children first)))

            #_(data/diff (-> layout :children first :children first)
                         (-> layout-3 :children first :children first))

            ;; (is (not (identical? (-> layout :node :children second :node)
            ;;                      (-> layout-3 :node :children second :node))))
            )
          ))))
  )

(deftest test-static-size
  (with-bindings (application/bindings)
    (is (= '{:children ({:width 10,
                         :height 50,
                         :x 0,
                         :y 0}),
             :width 100,
             :height 100,
             :x 0,
             :y 0}
           (scene-graph/dissoc-node-keys [:unplaced-node]
                                         (layout/layout-scene-graph {:children [{:width 10 :height 50}]}
                                                                    100 100))))))

(defn box [content]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color [0.3 0.3 1.0 1.0]
                                    :corner-arc-radius 20)
               content
               {:fill-width? true
                :fill-height? true}))

;; (defn root-view []
;;   (layouts/grid {:fill-width? true :fill-height? true}
;;                 [[(layouts/with-margin-2 {:margin 10 :fill-width? false :fill-height? true}
;;                     (box (text "hello")))

;;                   (layouts/with-margin-2 {:margin 10 :fill-width? true :fill-height? true}
;;                     (box (layouts/vertically-2 {:margin 10}
;;                                                (text "hello")
;;                                                (text "world !!!!"))))]

;;                  [(layouts/with-margin-2 {:margin 10 :fill-width? true :fill-height? true}
;;                     (box (layouts/vertically-2 {:margin 10}
;;                                                (text "hello")
;;                                                (text "world !!!!"))))

;;                   (layouts/with-margin-2 {:margin 10 :fill-width? true :fill-height? false}
;;                     (box (text "hello")))]]))

(defn root-view []
  (layouts/grid-2 {:gap 10
                   :padding 20
                   :fill-width? false
                   :fill-height? true
                   :cell-background (visuals/rectangle-2 :fill-color [0.3 0.3 1.0 1.0]
                                                         :corner-arc-radius 50)}
                  [[(visuals/rectangle-2 :fill-color [0.3 0.3 1.0 1.0]
                                         :line-width 0
                                         :corner-arc-radius 50
                                         :width 500
                                         :height 500)
                    (visuals/rectangle-2 :fill-color [0.3 1.0 0.3 1.0]
                                         :corner-arc-radius 50
                                         :width 500
                                         :height 500)]
                   [(box (text "hello"))

                    (layouts/vertically-2 {:margin 10}
                                          (text "hello")
                                          (text "world !!!!"))]

                   [(layouts/vertically-2 {:margin 10}
                                          (text "hello")
                                          (text "world !!!!"))

                    (box (text "hello"))]]))


;; (defn root-view []
;;   (layouts/horizontally-2 {:margin 10}
;;                           (layouts/vertically-2 {:margin 10}
;;                                                 (box (layouts/vertically-2 {:margin 10}
;;                                                                            (text "hello world !!!!")))
;;                                                 (box (layouts/vertically-2 {:margin 10}
;;                                                                            (text "hello"))))
;;                           (layouts/vertically-2 {:fill-width? true
;;                                                  :margin 10}
;;                                                 (box (layouts/vertically-2 {:margin 10}
;;                                                                            (text "hello world")))
;;                                                 (box (layouts/vertically-2 {:margin 10}
;;                                                                            (text "hello"))))))


(application/def-start root-view)

(comment

  (application/start-application (var root-view))

  (with-bindings (application/create-bindings-without-window [root-view])
    (application/handle-events! [{:type :resize-requested, :width 2000, :height 2000}]))



  (with-bindings (application/bindings)
    (application/create-scene-graph [root-view]))

  ) ;; TODO: remove me

(deftest test-with-margin
  (with-bindings (application/bindings)
    (let [root-view (fn [] (layouts/with-margin 50
                             (layouts/vertically-2 {:margin 10}
                                                   (text "hello"))))
          scene-graph (view-compiler/compile-view-calls [root-view])

          layout (layout/layout-scene-graph scene-graph
                                                  500
                                                  500)]

      #_(application/handle-new-scene-graph! (layout/apply-layout-nodes layout))

      ;; (is (= (remove-indentation "{:type :fungl.layouts/with-margins}
      ;;                               {:type :flow-gl.gui.visuals/text-area, :string \"foo\"}
      ;;                            ")
      ;;        (scene-graph-to-string scene-graph)))
      #_(is (= (remove-indentation "{:x 0, :y 0, :width 500, :height 500}
                                    {:width 110.0, :height 76.640625, :type :fungl.layouts/with-margins}
                                      {:x 10, :y 10, :width 90.0, :height 56.640625}
                                        {:width 90.0, :height 56.640625, :type :flow-gl.gui.visuals/text-area, :string \"foo\"}
                                 ")
               (scene-graph-to-string layout)))

      #_(application/handle-events! [{:type :resize-requested, :width 2000, :height 2000}])

      #_(scene-graph/containing-roots layout)))
  )

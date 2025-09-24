(ns fungl.examples.focus
  (:require
   [clj-async-profiler.core :as clj-async-profiler]
   [clojure.test :refer :all]
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.command :as command]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.scene-graph :as scene-graph]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.derivation :as derivation]
   [fungl.layouts :as layouts]
   [fungl.view-compiler :as view-compiler]
   [medley.core :as medley]))


(comment
  (clj-async-profiler/serve-ui 9898)

  (clj-async-profiler/start {:event :cpu #_:alloc
                             :threads true
                             :interval (let [framerate 1000
                                             millisecond 1000000]
                                         (/ (* 1000 millisecond)
                                            framerate))})

  (clj-async-profiler/stop)

  )

;; TODO: cache with scene-graph/flatten is slow because it compares scenegraphs, identity cache should be used instead
;; flatten is used to find the next focusable node
;; flow-gl.gui.command/command-handler-keyboard-event-handler
(def font (font/create-by-name "CourierNewPSMT" 20))

(defn focus-box [message]
  (let [sub-component-is-in-focus? #_(keyboard/sub-component-is-in-focus?)
        @(derivation/derive "subcomponent-is-in-focus"
                            (let [id view-compiler/id]
                              (fn []
                                (= id
                                   (take (count id)
                                         @keyboard/focused-node-id-derivation)))))]
    (-> (layouts/box 10
                     (visuals/rectangle-2 :fill-color (if sub-component-is-in-focus?
                                                        [200 200 255 255]
                                                        [0.3 0.3 0.3 1.0])
                                          :corner-arc-radius 30)
                     (visuals/text message
                                   {:color (if sub-component-is-in-focus?
                                             [0 0 0 255]
                                             [1.0 1.0 1.0 1.0])
                                    :font font}))
        (assoc :can-gain-focus? true))))

(defn root-view []
  (let [state-atom (dependable-atom/atom {:y 0})]
    (fn []
      [command/command-handler {:node (layouts/superimpose (visuals/rectangle-2 :fill-color [0 0 0 255])
                                                           (layouts/fill :down
                                                                         (visuals/text "hello")
                                                                         (visuals/clip {:node (layouts/vertically-2 {:margin 20}
                                                                                                                    (for [index (range 100)]
                                                                                                                      [focus-box (str "focus-box " index)]))
                                                                                        :y (:y @state-atom)
                                                                                        :local-id :scrolling-pane})))
                                :command-set {:name "root"
                                              :commands [{:name "move focus down"
                                                          :available? true
                                                          :key-patterns [[[#{:control} :n]]
                                                                         #_[[#{:meta} :n]]]
                                                          :run! (fn [_subtree]
                                                                  (keyboard/select-node-and-move-focus! scene-graph/closest-node-down))}

                                                         {:name "move focus down 10 times"
                                                          :available? true
                                                          :key-patterns [[[#{:control :meta} :n]]
                                                                         #_[[#{:meta} :n]]]
                                                          :run! (fn [_subtree]
                                                                  (dotimes [_ 10]
                                                                    (keyboard/select-node-and-move-focus! scene-graph/closest-node-down)))}

                                                         {:name "move focus up"
                                                          :available? true
                                                          :key-patterns [[#{:control} :p]]
                                                          :run! (fn [_subtree]
                                                                  (keyboard/select-node-and-move-focus! scene-graph/closest-node-up))}

                                                         {:name "move focus up 10 times"
                                                          :available? true
                                                          :key-patterns [[[#{:control :meta} :p]]]
                                                          :run! (fn [_subtree]
                                                                  (dotimes [_ 10]
                                                                    (keyboard/select-node-and-move-focus! scene-graph/closest-node-up)))}

                                                         {:name "scroll to focus"
                                                          :available? @keyboard/focused-node-id-derivation
                                                          :key-patterns [[#{:control} :l]]
                                                          :run! (fn [_subtre]
                                                                  (let [focused-path (scene-graph/path-to @scene-graph/current-scene-graph-atom
                                                                                                          @keyboard/focused-node-id-derivation)
                                                                        scrolling-pane (medley/find-first #(= :scrolling-pane
                                                                                                              (:local-id %))
                                                                                                          focused-path)
                                                                        focused-node-y-in-scrolling-pane (reduce + (map :y (drop 1 (drop-while #(not (= :scrolling-pane
                                                                                                                                                        (:local-id %)))
                                                                                                                                               focused-path))))
                                                                        middle-y (/ (:height scrolling-pane)
                                                                                    2)
                                                                        middle-scroll-y (- middle-y
                                                                                           (/ (:height (last focused-path))
                                                                                              2)
                                                                                           focused-node-y-in-scrolling-pane)]
                                                                    (swap! state-atom assoc :y (if (= (:y @state-atom)
                                                                                                      middle-scroll-y)
                                                                                                 (+ 20 (- focused-node-y-in-scrolling-pane))
                                                                                                 middle-scroll-y) )))}]}}])))


(application/def-start root-view)

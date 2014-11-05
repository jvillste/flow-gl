(ns examples.render-targets-with-renderers
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]
                                 [quad :as quad])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [renderer :as renderer]
                         [transformer :as transformer]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 60]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))


(defn text [text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 30)
                   [1 1 1 1]))


(defn layout-for-time [time]
  (let [duration 5000
        phase (/ (mod time duration)
                 duration)
        pulse-phase (Math/sin (* phase Math/PI))]
    (let [[state layout] (layout/layout (-> (transformer/with-transformers
                                              (transformer/->Highlight :highlight)
                                              (transformer/->Filter :fade1
                                                                    quad/alpha-fragment-shader-source
                                                                    [:1f "alpha" pulse-phase])
                                              (layouts/->VerticalStack
                                               [(text "child 1")
                                                (assoc (text "child 2") :highlight? true)
                                                (-> (transformer/with-transformers (transformer/->Filter :fade2
                                                                                                         quad/alpha-fragment-shader-source
                                                                                                         [:1f "alpha" 0.7])
                                                      (text "child 3")))]))

                                            (assoc :width 200
                                                   :height 200
                                                   :x 0
                                                   :y 0))

                                        {}
                                        200 200)]
      layout)))


(defn start-view []
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)
        render-tree-state-atom (atom nil)]

    (try
      (loop []
        (let [frame-started (System/currentTimeMillis)
              render-trees (transformer/render-trees-for-layout
                            (layout-for-time frame-started))]
          (window/set-display window gl
                              (opengl/clear gl 0 0 0 1)
                              (let [{:keys [width height]} (opengl/size gl)]
                                (swap! render-tree-state-atom
                                       (fn [render-tree-state]
                                         (let [[render-tree-state drawables] (transformer/transform-tree render-tree-state
                                                                                                         {:transformers [(transformer/->RenderTransformer :root)]
                                                                                                          :children render-trees
                                                                                                          :width width
                                                                                                          :height height
                                                                                                          :x 0
                                                                                                          :y 0}
                                                                                                         gl)]
                                           render-tree-state)))))

          (when (window/visible? window)
            (do (wait-for-next-frame frame-started)
                (recur)))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view))

(run-tests)

#_( transform drawables to multiple textures with filtering and transposing)

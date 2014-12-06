(ns examples.render-targets-with-renderers
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]
                                 [quad :as quad])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            (flow-gl.gui [drawable :as drawable]
                         [window :as window]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [renderer :as renderer]
                         [transformer :as transformer]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))



(defn text [text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 30)
                   [1 1 1 1]))

(defn layoutable-for-time [time]
    (let [duration 3000
          phase (/ (mod time duration)
                   duration)
          pulse-phase (Math/sin (* phase Math/PI))]
      (transformer/with-transformers
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
                (text "child 3")))]))))

#_(defn layoutable-for-time [time]
  (layouts/->FloatLeft (layouts/->HorizontalStack [(drawable/->Rectangle 10 10 [255 0 0 255])
                                                   #_(drawable/->Rectangle 10 10 [0 255 0 255])
                                                   (layouts/->Margin 0 0 0 10
                                                                     [(layouts/->VerticalStack [(drawable/->Rectangle 10 10 [0 255 0 255])
                                                                                                #_(drawable/->Rectangle 10 10 [255 255 0 255])]) ])] )
                       (drawable/->Rectangle 10 10 [0 0 255 255])))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 60]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))



(deftest test
  (is (= nil
         (let [width 300
               height 300]
           (-> (layoutable-for-time 0)
               (assoc :width width
                      :height height
                      :x 0
                      :y 0)
               (layout/layout {}
                              width
                              height)
               (second)
               (transformer/render-trees-for-layout))))))


(defn start-view []
  (let [width 300
        height 300
        window (jogl-window/create width
                              height
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)
        render-tree-state-atom (atom nil)]

    (try
      (loop []
        (let [frame-started (System/currentTimeMillis)
              render-trees (-> (layoutable-for-time frame-started)
                               (assoc :width width
                                      :height height
                                      :x 0
                                      :y 0)
                               (layout/layout {}
                                              width
                                              height)
                               (second)
                               (transformer/render-trees-for-layout))]
          (window/render-constantly window gl
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

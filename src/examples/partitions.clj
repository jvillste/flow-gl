(ns examples.partitions
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [quad-batch :as quad-batch]
                                 [window :as jogl-window]
                                 [render-target :as render-target])
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
                         [layout-dsl :as l]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defn set-size [drawable]
  (let [preferred-size (layoutable/preferred-size drawable 1000 1000)]
    (assoc drawable
      :width (:width preferred-size)
      :height (:height preferred-size))))

(defn text [text]
  (set-size (drawable/->Text text
                             (font/create "LiberationSans-Regular.ttf" 54)
                             [255 255 255 255])))

(defn layoutable-for-time [time]
  (let [length 5000
        phase (/ (mod time length)
                 length)]
    (l/vertically (l/horizontally (text "foo") (text "bar") (text "baz"))
                  (text (str "phase: " (format "%.2f" (float phase))))
                  (l/horizontally (text "Foo") (text "Bar") (text "Baz")))))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 1]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn do-layout [layoutable width height]
  (assoc (second (layout/layout layoutable
                                {} width height))
    :x 0 :y 0 :z 0 :width width :height height))

(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize
                                   :close-automatically true)]

    (try
      (loop [gpu-state (gui/initialize-gpu-state window)]
        (println "staring frame")
        (when (window/visible? window)
          (let [frame-started (System/currentTimeMillis)
                layout (do-layout (layoutable-for-time frame-started)
                                  (window/width window)
                                  (window/height window))
                gpu-state (window/with-gl window gl
                            (opengl/clear gl 0 0 0 1)
                            (gui/render gpu-state layout))]

            (wait-for-next-frame frame-started)

            (recur gpu-state))))


      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view))

(run-tests)

(println (gui/partition-by-differences (do-layout (layoutable-for-time 0)
                                                  100 100)
                                       (do-layout (layoutable-for-time 100)
                                                  100 100)) )

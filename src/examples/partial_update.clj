(ns examples.partial-update
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
  (set-size (drawable/->Text (str text)
                             (font/create "LiberationSans-Regular.ttf" 54)
                             [255 255 255 255])))

(defn layoutable-for-time [time]
  (let [length 500
        phase (/ (mod time length)
                 length)]
    (l/box 10
           (drawable/->Rectangle 0 0 [0 255 0 255])
           (l/vertically (l/horizontally (text "1") (text "2") (text "3"))
                         (l/horizontally (text "4") (text (apply str (repeat (* phase 2) "x"))) (text "ABCD"))
                         (l/horizontally (text "7") (text "8") (text "9"))))
    
    #_(layouts/->Preferred (l/box 10
                                  (drawable/->Rectangle 0 0 [0 255 0 255])
                                  (l/vertically (l/horizontally (text "1") (text "2") (text "3"))
                                                (l/horizontally (text "4") (text (apply str (repeat (* phase 2) "x"))) (text "ABCD"))
                                                (l/horizontally (text "7") (text "8") (text "9")))))))

(defn wait-for-next-frame [frame-started target-frames-per-second]
  (Thread/sleep (max 0
                     (- (/ 1000 target-frames-per-second)
                        (- (System/currentTimeMillis)
                           frame-started)))))

(defn do-layout [layoutable width height]
  (assoc (second (layout/layout layoutable
                                {} width height))
    :x 0 :y 0 :z 0 :width width :height height))

(defn start-view []
  (let [window (jogl-window/create 600
                                   600
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize
                                   :close-automatically true)]

    (try
      (loop [gpu-state (gui/initialize-gpu-state window)]
        (when (window/visible? window)
          (let [frame-started (System/currentTimeMillis)
                layout (do-layout (layoutable-for-time frame-started)
                                  (window/width window)
                                  (window/height window))
                gpu-state (window/with-gl window gl
                            (gui/render-frame (assoc gpu-state :layout layout)))]

            (window/swap-buffers window)

            (wait-for-next-frame frame-started 10)

            (recur gpu-state))))


      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view))

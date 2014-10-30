(ns examples.render-targets-with-renderers
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]
                                 [texture :as texture]
                                 [quad :as quad])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [renderer :as renderer]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 60]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn drawable-for-time [texture time]
  (let [duration 2000
        phase (/ (mod time duration)
                 duration)]

    (if (< phase 0.5)
      (drawable/->Quad ["texture" texture]
                       [:1f "alpha" phase]
                       quad/alpha-fragment-shader-source
                       0 0 200 200)

      (drawable/->Quad ["texture" texture]
                       []
                       quad/fragment-shader-source
                       0 0 200 200))))

(defn start-view []
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)
        renderers (atom [(window/with-gl window gl
                           (renderer/create-quad-renderer gl))])
        texture (window/with-gl window gl
                  (texture/create-for-file "pumpkin.png" gl))]

    (try
      (loop []
        (let [frame-started (System/currentTimeMillis)]
          (let [drawable (drawable-for-time texture frame-started)]
            (window/set-display window gl
                                (opengl/clear gl 0 0 0 1)
                                (swap! renderers
                                       (fn [renderers]
                                         (renderer/render-frame [drawable] gl renderers)))))

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

#_( render drawables to multiple textures with filtering and transposing)

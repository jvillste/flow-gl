(ns examples.nanovg
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
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
  (let [target-frames-per-second 5]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn start-view [drawables-for-time]
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)]


    (try
      (let [renderers-atom (atom (window/with-gl window gl [(renderer/create-render-target-renderer [(renderer/create-nanovg-renderer)
                                                                                                     (renderer/create-quad-view-renderer gl)]
                                                                                                    gl)]))]
        (loop []
          (let [frame-started (System/currentTimeMillis)]
            (let [drawables (drawables-for-time frame-started)]
              (window/set-display window gl
                                  (opengl/clear gl 0 0 0 1)
                                  (reset! renderers-atom
                                          (gui/render-frame drawables gl @renderers-atom))))

            (when (window/visible? window)
              (do (wait-for-next-frame frame-started)
                  (recur))))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(def image (buffered-image/create-from-file "pumpkin.png"))

(defn set-size [drawable]
  (let [preferred-size (layoutable/preferred-size drawable 1000 1000)]
    (assoc drawable
      :width (:width preferred-size)
      :height (:height preferred-size))))

(defn text [text]
  (set-size (drawable/->Text text
                             (font/create "LiberationSans-Regular.ttf" 54)
                             [1 1 1 1])))

(defn drawables-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]
    [{:render-target? true
      :child-drawables [(assoc (text "foo 3")
                          :x (* 100 phase)
                          :y 140)

                        {:render-target? true
                         :child-drawables [(assoc (text "foo 2")
                                             :x (* 100 phase)
                                             :y 0)]}]}

     #_(assoc (set-size (drawable/->Image image))
            :x (* 100 phase)
            :y 40)

     #_(assoc (drawable/->Rectangle 10 100 #_[1 0 0 1] [255 0 0 255])
       :x (* 100 phase)
       :y 0)]))

(defn start []
  (start-view drawables-for-time))

(run-tests)

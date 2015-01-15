(ns examples.render-on-quad
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
                         [renderer :as renderer]))
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

(defn drawables-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]

    [(assoc (set-size (text "FOO"))
       :x (* phase 10)
       :y 10)]))


(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 60]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize
                                   :close-automatically true)]


    (try
      (let [render-target (window/with-gl window gl
                            (render-target/create 50 50
                                                  gl))
            gpu-state (gui/initialize-gpu-state window)
            drawable {:has-predefined-texture true}
            gpu-state (window/with-gl window gl
                        (render-target/render-to render-target gl
                                                 (opengl/clear gl 1 0 0 1)
                                                 (-> gpu-state
                                                     (gui/layout-to-render-trees partition)
                                                     (gui/render-trees-to-drawables)
                                                     (gui/render-drawables))))]



        (let [quad-view-renderer :foo #_(window/with-gl window gl
                                          (update-in quad-view-renderer
                                                     [:quad-view]
                                                     quad-view/add-gl-texture
                                                     drawable
                                                     (:texture render-target)
                                                     (:width render-target)
                                                     (:height render-target)
                                                     gl))]
          (loop [quad-view-renderer quad-view-renderer]
            (when (window/visible? window)
              (let [frame-started (System/currentTimeMillis)]

                (let [drawables  [(assoc drawable :x 0 :y 0 :z 0)
                                  (assoc drawable :x 0 :y 100 :z 0)]]
                  (let [[quad-view-renderer] (window/with-gl window gl
                                               (opengl/clear gl 0 0 0 1)
                                               (renderer/render-frame drawables gl [quad-view-renderer]))]

                    (window/swap-buffers window)
                    (wait-for-next-frame frame-started)
                    (recur quad-view-renderer))))))))


      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view))

(run-tests)

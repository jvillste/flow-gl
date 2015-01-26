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

(defn drawables-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]

    [(assoc (set-size (text "FOO"))
       :x (* phase 10)
       :y 10)]))


(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 10]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn draw-rectangle [nanovg x y width height r g b a]
  (doto nanovg
    (NanoVG/fillColor (char r) (char g) (char b) (char a))
    (NanoVG/beginPath)
    (NanoVG/moveTo x y)
    (NanoVG/lineTo width height)
    (NanoVG/lineTo 0 height)
    (NanoVG/closePath)
    ;;(NanoVG/rect x y width height)
    (NanoVG/fill)))

(defn quad-batch-status [gpu-state]
  (let [quad-batch (get-in gpu-state [:renderers :quad-view :quad-view :quad-batch])]
    (println "quad-batch" (select-keys quad-batch [:removed-texels
                                                   :allocated-texels
                                                   #_:next-free-texture-id
                                                   #_:next-free-texel
                                                   #_:textures-in-use])))
  gpu-state)

(defn add-gl-texture [gpu-state window drawable]
  (let [size (layoutable/preferred-size drawable Integer/MAX_VALUE Integer/MAX_VALUE)
        render-target (window/with-gl window gl
                        (render-target/create (:width size) (:height size)
                                              gl))
        gpu-state (window/with-gl window gl
                    (render-target/render-to render-target gl
                                             (opengl/clear gl 1 0 0 1)
                                             (-> gpu-state
                                                 (assoc :drawables [drawable]
                                                        :gl gl)
                                                 (gui/render-drawables))))

        gpu-state (quad-batch-status (window/with-gl window gl
                                       (update-in gpu-state
                                                  [:renderers :quad-view :quad-view]
                                                  quad-view/add-gl-texture
                                                  drawable
                                                  (:texture render-target)
                                                  (:width render-target)
                                                  (:height render-target)
                                                  gl)))]
    (window/with-gl window gl
      (render-target/delete render-target gl))

    gpu-state))

(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize
                                   :close-automatically true)]

    (try
      (let [gpu-state (gui/initialize-gpu-state window)
            gpu-state (add-gl-texture gpu-state window (assoc (text "1") :x 0 :y 0 :z 0))
            gpu-state (add-gl-texture gpu-state window (assoc (text "2") :x 0 :y 0 :z 0))]

        (loop [gpu-state gpu-state]
          (when (window/visible? window)
            (let [frame-started (System/currentTimeMillis)
                  gpu-state (add-gl-texture gpu-state window (assoc (text (str "Phase is and was " (mod frame-started 10))) :x 0 :y 0 :z 0))
                  drawables [(assoc (text "1")  :x 0 :y 0 :z 0)
                             (assoc (text (str "Phase is and was " (mod frame-started 10))) :x 110 :y 0 :z 0)
                             (assoc (text "2")  :x 0 :y 110 :z 0)]
                  gpu-state (quad-batch-status (window/with-gl window gl
                                                 (opengl/clear gl 0 0 0 1)
                                                 (-> gpu-state
                                                     (assoc :drawables drawables
                                                            :gl gl)
                                                     (gui/start-frame)
                                                     (gui/render-drawables)
                                                     (gui/end-frame))))]
              (window/swap-buffers window)
              (wait-for-next-frame frame-started)
              (recur gpu-state)))))


      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view))

(run-tests)

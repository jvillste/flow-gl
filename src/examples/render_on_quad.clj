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
  (let [target-frames-per-second 1]
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
                                                   :next-free-texture-id
                                                   :next-free-texel
                                                   :textures-in-use])))
  gpu-state)

(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize
                                   :close-automatically true)]

    (try
      (let [render-target (window/with-gl window gl
                            (render-target/create 100 100
                                                  gl))
            gpu-state (quad-batch-status (gui/initialize-gpu-state window))
            drawable {:has-predefined-texture true}
            layout (assoc (second (layout/layout (l/vertically (text "foo") (text "bar")) {} 100 100))
                     :x 0 :y 0 :z 0)
            #_nanovg  #_(window/with-gl window gl
                          (NanoVG/init))

            #_quad-batch #_(window/with-gl window gl
                             (-> (quad-batch/create gl)
                                 (quad-batch/add-textures gl (quad-view/create-textures [(set-size (text "Faa"))]))))

            #_quad-batch #_(window/with-gl window gl
                             (render-target/render-to render-target gl
                                                      (opengl/clear gl 0 1 0 1)
                                                      (quad-batch/draw-quads quad-batch
                                                                             gl
                                                                             [{:x 0 :y 0 :width 100 :height 100 :texture-id 0}]
                                                                             (:width render-target) (:height render-target))
                                                      (quad-batch/add-textures quad-batch gl (quad-view/create-textures [(set-size (text "Fee"))]))))

            gpu-state (quad-batch-status (window/with-gl window gl
                                           (render-target/render-to render-target gl
                                                                    (opengl/clear gl 0 0 0 1)

                                                                    #_(NanoVG/beginFrame nanovg (:width render-target)
                                                                                         (:height render-target))
                                                                    #_(draw-rectangle nanovg
                                                                                      0 0 (:width render-target) (:height render-target)
                                                                                      0 255 0 255)
                                                                    #_(NanoVG/endFrame nanovg)

                                                                    #_gpu-state
                                                                    #_(update-in gpu-state [:renderers :quad-view :quad-view]
                                                                                 quad-view/draw-drawables
                                                                                 [(assoc (text "haa") :x 0 :y 0 :z 0)]
                                                                                 (:width render-target) (:height render-target) gl)
                                                                    (-> gpu-state
                                                                        (assoc :drawables [(assoc (text "haa") :x 0 :y 0 :z 0)]
                                                                               :gl gl)
                                                                        (gui/start-frame)
                                                                        (gui/render-drawables)
                                                                        (gui/end-frame)))))


            #_quad-batch #_(window/with-gl window gl
                             (quad-batch/add-textures-from-gl-textures
                              quad-batch
                              gl
                              [{:width (:width render-target)
                                :height (:height render-target)
                                :texture-id (:texture render-target)}]))

            gpu-state (quad-batch-status (window/with-gl window gl
                                           (update-in gpu-state
                                                      [:renderers :quad-view :quad-view]
                                                      quad-view/add-gl-texture
                                                      drawable
                                                      (:texture render-target)
                                                      (:width render-target)
                                                      (:height render-target)
                                                      gl)))]

        (loop [gpu-state gpu-state]
          (when (window/visible? window)
            (let [frame-started (System/currentTimeMillis)
                  drawables [(assoc drawable :x 0 :y 0 :z 0)
                             #_(assoc (text "haa") :x 0 :y 0 :z 0)
                             (assoc drawable :x 0 :y 110 :z 0)]
                  gpu-state (quad-batch-status (window/with-gl window gl
                                                 (opengl/clear gl 0 0 0 1)
                                                 #_(NanoVG/beginFrame nanovg 300 400)
                                                 #_(draw-rectangle nanovg
                                                                   0 0 300 400
                                                                   0 255 0 255)
                                                 #_(NanoVG/endFrame nanovg)

                                                 #_(quad-batch/draw-quads quad-batch
                                                                          gl
                                                                          [{:x 0 :y 0 :width 100 :height 100 :texture-id 0}
                                                                           {:x 0 :y 110 :width 100 :height 100 :texture-id 1}]
                                                                          300 400)
                                                 #_gpu-state
                                                 #_(update-in gpu-state [:renderers :quad-view :quad-view]
                                                              quad-view/draw-drawables
                                                              drawables
                                                              300 400 gl)

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

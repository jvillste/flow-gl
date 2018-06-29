(ns examples.stencil-on-fbo
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [render-target :as render-target]
                                 [frame-buffer :as frame-buffer]
                                 [stencil :as stencil]
                                 [multicolor-triangle-list :as multicolor-triangle-list])
            [flow-gl.opengl.math :as math]
            (flow-gl.gui [window :as window]))
  (:use clojure.test)
  (:import [nanovg NanoVG]
           [com.jogamp.opengl GL2]))

(defn draw-rectangle [nanovg x y width height r g b a]
  (doto nanovg
    (NanoVG/fillColor (char r) (char g) (char b) (char a))
    (NanoVG/beginPath)
    (NanoVG/moveTo 10 10)
    (NanoVG/lineTo 100 100)
    (NanoVG/lineTo 10 100)
    (NanoVG/closePath)
    ;;(NanoVG/rect x y width height)
    (NanoVG/fill)))

(defn render [triangle-list nanovg width height gl]
  (opengl/clear gl 0 0 0 1)
  (stencil/set [{:x 0 :y 0 :width 50 :height 50}] gl)

  (multicolor-triangle-list/set-size triangle-list width height gl)
  (multicolor-triangle-list/render-coordinates triangle-list
                                    (math/quad 0 0 width height)
                                    [1 0 1 1
                                     0 1 0 1
                                     0 0 1 1

                                     1 0 1 1
                                     1 0 1 1
                                     1 0 1 1]
                                    gl)
  
  #_(NanoVG/beginFrame nanovg width height)
  #_(draw-rectangle nanovg
                  0 0 100 100
                  0 255 0 255)
  #_(NanoVG/endFrame nanovg)
  (stencil/disable gl))

(defn start-view []
  (let [window (jogl-window/create 200
                                   200
                                   :profile :gl3
                                   :init opengl/initialize
                                   :close-automatically true)
        render-target-width 200
        render-target-height 200

        render-target (window/with-gl window gl
                        (render-target/create render-target-width render-target-height
                                              gl))

        triangle-list (window/with-gl window gl
                        (multicolor-triangle-list/create gl :triangles))
        
        nanovg (window/with-gl window gl
                 (NanoVG/init))]

    (try
      (window/with-gl window gl
        (let [{:keys [width height]} (opengl/size gl)]
          (opengl/clear gl 0 0 0 1)

          (render-target/render-to render-target gl
                                   (render triangle-list nanovg render-target-width render-target-height gl))

          (render-target/blit render-target gl)))
      
      (window/swap-buffers window)

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))


(defn start []
  (start-view))

(run-tests)

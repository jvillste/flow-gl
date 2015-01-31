(ns examples.stencil
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [render-target :as render-target]
                                 [frame-buffer :as frame-buffer]
                                 [triangle-list :as triangle-list])
            (flow-gl.gui [window :as window]))
  (:use clojure.test)
  (:import [nanovg NanoVG]
           [javax.media.opengl GL2]))

(defn draw-rectangle [nanovg x y width height r g b a]
  (doto nanovg
    (NanoVG/fillColor (char r) (char g) (char b) (char a))
    (NanoVG/beginPath)
    (NanoVG/moveTo 10 10)
    (NanoVG/lineTo 100 100)
    (NanoVG/lineTo 10 100)
    (NanoVG/closePath)
    (NanoVG/rect x y width height)
    (NanoVG/fill)))

(defn quad [x y width height]
  [x y
   x (+ y height)
   (+ x width) y

   x (+ y height)
   (+ x width) (+ y height)
   (+ x width) y])

(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :close-automatically true)

        nanovg  (window/with-gl window gl
                  (NanoVG/init))]

    (window/with-gl window gl
      (triangle-list/create-shared-resources gl))

    (let [triangle-list (window/with-gl window gl
                          (triangle-list/create gl :triangle-strip))]
      (try
        (window/with-gl window gl
          (let [{:keys [width height]} (opengl/size gl)]
            (opengl/clear gl 0 0 0 1)

            (triangle-list/set-size triangle-list width height gl)

            (doto gl
              (.glColorMask false false false false)
              (.glEnable GL2/GL_STENCIL_TEST)
              (.glClearStencil 0)
              (.glClear GL2/GL_STENCIL_BUFFER_BIT)
              (.glStencilFunc GL2/GL_ALWAYS 1 1)
              (.glStencilOp GL2/GL_REPLACE GL2/GL_REPLACE GL2/GL_REPLACE))

            (triangle-list/render-single-color-coordinates triangle-list
                                                           [0 0
                                                            50 0
                                                            0 100]
                                                           [1 0 0 1]
                                                           gl)


            #_(triangle-list/render-coordinates triangle-list
                                                [0 0
                                                 50 0
                                                 0 100]
                                              [1 0 0 1
                                               0 1 0 1
                                               0 0 1 1]
                                              gl)

            (doto gl
              (.glStencilFunc GL2/GL_EQUAL 1 1)
              (.glColorMask true true true true)
              (.glStencilOp GL2/GL_KEEP GL2/GL_KEEP GL2/GL_KEEP))



            (triangle-list/render-coordinates triangle-list
                                              [0 0
                                               100 0
                                               0 100]
                                              [1 0 0 1
                                               0 1 0 1
                                               0 0 1 1]
                                              gl)

            (.glDisable gl GL2/GL_STENCIL_TEST)))

        (window/swap-buffers window)

        (println "exiting")
        (catch Exception e
          (println "exception")
          (window/close window)
          (throw e))))))


(defn start []
  (start-view))

(run-tests)

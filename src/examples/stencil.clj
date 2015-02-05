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

(defn quad [{:keys [x y width height]}]
  [x y
   x (+ y height)
   (+ x width) y

   x (+ y height)
   (+ x width) (+ y height)
   (+ x width) y])

(defn set-stencil [rectangles gl]
  (doto gl
    (.glColorMask false false false false)
    (.glEnable GL2/GL_STENCIL_TEST)
    (.glClearStencil 0)
    (.glClear GL2/GL_STENCIL_BUFFER_BIT)
    (.glStencilFunc GL2/GL_ALWAYS 1 1)
    (.glStencilOp GL2/GL_REPLACE GL2/GL_REPLACE GL2/GL_REPLACE))

  (let [triangle-list (triangle-list/create gl :triangles)
        {:keys [width height]} (opengl/size gl)]

    (triangle-list/set-size triangle-list width height gl)

    (triangle-list/render-coordinates triangle-list
                                      (apply concat (map quad rectangles))
                                      [0 0 0 1]
                                      gl)
    
    (triangle-list/delete triangle-list gl))

  (doto gl
    (.glStencilFunc GL2/GL_EQUAL 1 1)
    (.glColorMask true true true true)
    (.glStencilOp GL2/GL_KEEP GL2/GL_KEEP GL2/GL_KEEP)))

(defn disable-stencil [gl]
  (.glDisable gl GL2/GL_STENCIL_TEST))

(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :close-automatically true)

        nanovg  (window/with-gl window gl
                  (NanoVG/init))]

    (let [triangle-list (window/with-gl window gl
                          (triangle-list/create gl :triangles))]
      (try
        (window/with-gl window gl
          (let [{:keys [width height]} (opengl/size gl)]
            (opengl/clear gl 0 0 0 1)

            (set-stencil [{:x 10 :y 10 :width 100 :height 100}
                          {:x 10 :y 120 :width 100 :height 100}]
                         gl)

            (triangle-list/set-size triangle-list width height gl)
            (triangle-list/render-coordinates triangle-list
                                              (quad {:x 0 :y 0 :width width :height height})
                                              [1 1 1 1]
                                              gl)


            (disable-stencil gl)))

        (window/swap-buffers window)

        (println "exiting")
        (catch Exception e
          (println "exception")
          (window/close window)
          (throw e))))))


(defn start []
  (start-view))

(run-tests)

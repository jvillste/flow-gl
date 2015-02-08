(ns flow-gl.opengl.jogl.stencil
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [triangle-list :as triangle-list]))

  (:import [javax.media.opengl GL2]))

(defn rectangle-to-triangle-coordinates [{:keys [x y width height]}]
  [x y
   x (+ y height)
   (+ x width) y

   x (+ y height)
   (+ x width) (+ y height)
   (+ x width) y])

(defn create [gl]
  {:triangle-list (triangle-list/create gl :triangles)})

(defn delete [stencil gl]
  (triangle-list/delete (:triangle-list stencil) gl))

(defn set [stencil rectangles gl]
  (doto gl
    (.glColorMask false false false false)
    (.glEnable GL2/GL_STENCIL_TEST)
    (.glClearStencil 0)
    (.glClear GL2/GL_STENCIL_BUFFER_BIT)
    (.glStencilFunc GL2/GL_ALWAYS 1 1)
    (.glStencilOp GL2/GL_REPLACE GL2/GL_REPLACE GL2/GL_REPLACE))

  (let [{:keys [width height]} (opengl/size gl)]

    (triangle-list/set-size (:triangle-list stencil) width height gl)

    (triangle-list/render-coordinates (:triangle-list stencil)
                                      (apply concat (map rectangle-to-triangle-coordinates rectangles))
                                      [0 0 0 1]
                                      gl))

  (doto gl
    (.glStencilFunc GL2/GL_EQUAL 1 1)
    (.glColorMask true true true true)
    (.glStencilOp GL2/GL_KEEP GL2/GL_KEEP GL2/GL_KEEP)))

(defn disable [gl]
  (.glDisable gl GL2/GL_STENCIL_TEST))

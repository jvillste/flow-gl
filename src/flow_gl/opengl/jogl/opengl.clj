(ns flow-gl.opengl.jogl.opengl
  (:require (flow-gl.opengl.jogl [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]))
  (:import [javax.media.opengl GL2]))


(defn projection-matrix-2d [width height depth]
  [(/ 2.0 width)
   0.0
   0.0
   0.0

   0.0
   (/ -2.0 height)
   0.0
   0.0

   0.0
   0.0
   (/ 2.0 depth)
   0.0

   -1.0
   1.0
   1.0
   1.0])

(defn initialize-gl [gl]
  (doto gl
    (.glEnable GL2/GL_BLEND)
    ;;(.glEnable GL2/GL_TEXTURE_2D)
    (.glColorMask true, true, true, true)
    (.glBlendFunc GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA)))

(defn initialize [gl]
  (initialize-gl gl)
  (triangle-list/create-shared-resources gl)
  (textured-quad/create-shared-resources gl))


(defn dispose [gl]
  (triangle-list/delete-shared-resources gl)
  ;;(textured-quad/delete-shared-resources gl)
  )

(defn clear [gl r g b a]
  (doto gl
    (.glClearColor r g b a)
    (.glClear GL2/GL_COLOR_BUFFER_BIT)))

(defn resize [gl width height]
  ;; http://www.opengl.org/discussion_boards/showthread.php/172280-Constructing-an-orthographic-matrix-for-2D-drawing
  #_(doto gl
    (.glViewport 0 0 width height)
    (.glMatrixMode GL2/GL_PROJECTION)
    (.glLoadIdentity)
    (.glOrtho 0, width, 0, height, -1, 1)

    (.glMatrixMode GL2/GL_MODELVIEW)
    (.glLoadIdentity)
    (.glScalef 1 -1 1)
    (.glTranslatef 0 (- height) 0)))



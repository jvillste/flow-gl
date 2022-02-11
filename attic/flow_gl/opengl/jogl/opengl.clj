(ns flow-gl.opengl.jogl.opengl
  (:import (com.jogamp.opengl GL2)))




(defn initialize-gl [gl]
  (doto gl

    
    (.glEnable GL2/GL_BLEND)
    #_(.glColorMask true, true, true, true)
    #_(.glBlendFunc GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA)
    (.glBlendFuncSeparate GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA GL2/GL_ONE GL2/GL_ONE_MINUS_SRC_ALPHA)))

(defn initialize [gl]
  (initialize-gl gl)
  ;; (triangle-list/create-shared-resources gl)
  ;; (textured-quad/create-shared-resources gl)
  )


(defn dispose [gl]
  #_(triangle-list/delete-shared-resources gl)
  #_(textured-quad/delete-shared-resources gl))

(defn size [gl]
  (let [result-buffer (int-array 4)]
    (.glGetIntegerv gl GL2/GL_VIEWPORT result-buffer 0)
    {:width (aget result-buffer 2) :height (aget result-buffer 3)}))

(defn clear [gl r g b a]
  (doto gl
    (.glClearColor r g b a)
    (.glClear GL2/GL_COLOR_BUFFER_BIT)))

(defn copy-back-to-front [gl]
  (let [{:keys [width height]} (size gl)]
    (doto gl
      #_(.glReadBuffer GL2/GL_BACK)
      #_(.glDrawBuffer GL2/GL_FRONT)
      (.glBindFramebuffer GL2/GL_READ_FRAMEBUFFER 0)
      
      (.glBlitFramebuffer 0 0 width height 0 0 100 100 #_width #_height GL2/GL_COLOR_BUFFER_BIT GL2/GL_LINEAR)
      #_(.glRasterPos2i 10 10) 
      #_(.glCopyPixels 10 10 100 100 #_width #_height GL2/GL_COLOR))))



(defn clear-rectangle [gl x y width height r g b a]
  (let [framebuffer-height (:height (size gl))]
    (doto gl
      (.glEnable GL2/GL_SCISSOR_TEST)
      (.glScissor x (- framebuffer-height y height) width height)
      (.glClearColor r g b a)
      (.glClear GL2/GL_COLOR_BUFFER_BIT)
      (.glDisable GL2/GL_SCISSOR_TEST))))

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

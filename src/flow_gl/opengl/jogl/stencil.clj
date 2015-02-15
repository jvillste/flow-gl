(ns flow-gl.opengl.jogl.stencil
  (:refer-clojure :exclude [set])
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [triangle-list :as triangle-list])
            [flow-gl.graphics.native-buffer :as native-buffer])

  (:import [javax.media.opengl GL2]))

(defn coordinates-buffer [buffer rectangles]
  (let [number-of-coordinates (* 12 (count rectangles))
        buffer (native-buffer/ensure-buffer-capacity buffer
                                                     number-of-coordinates)]

    (doseq [rectangle rectangles]
      (let [{:keys [x y width height]} rectangle]
        (.put buffer
              (float-array [x y
                            x (+ y height)
                            (+ x width) y

                            x (+ y height)
                            (+ x width) (+ y height)
                            (+ x width) y]))))
    (.rewind buffer)

    buffer))

(defn create [gl]
  {:triangle-list (triangle-list/create gl :triangles)
   :buffer (native-buffer/create-native-buffer :float 256)})

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

  (let [{:keys [width height]} (opengl/size gl)
        buffer (coordinates-buffer (:buffer stencil) rectangles)]

    (triangle-list/set-size (:triangle-list stencil) width height gl)

    (triangle-list/render-coordinates-from-native-buffer (:triangle-list stencil)
                                                         buffer
                                                         [0 0 0 1]
                                                         gl)

    (doto gl
      (.glStencilFunc GL2/GL_EQUAL 1 1)
      (.glColorMask true true true true)
      (.glStencilOp GL2/GL_KEEP GL2/GL_KEEP GL2/GL_KEEP))

    (assoc stencil :buffer buffer)))

(defn disable [gl]
  (.glDisable gl GL2/GL_STENCIL_TEST))











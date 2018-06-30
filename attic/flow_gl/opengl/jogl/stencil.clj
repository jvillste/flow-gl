(ns flow-gl.opengl.jogl.stencil
  (:require [flow-gl.graphics.native-buffer :as native-buffer]
            [flow-gl.opengl.jogl.opengl :as opengl]
            [flow-gl.opengl.jogl.triangle-list :as triangle-list])
  (:import (com.jogamp.opengl GL2))
  (:refer-clojure :exclude [set]))

(defn coordinates-buffer [buffer rectangles]
  (native-buffer/ensure-buffer-capacity-with-values buffer
                                                    (mapcat (fn [{:keys [x y width height]}]
                                                              [x y
                                                               x (+ y height)
                                                               (+ x width) y

                                                               x (+ y height)
                                                               (+ x width) (+ y height)
                                                               (+ x width) y])
                                                            rectangles))
  
  #_(let [number-of-coordinates (* 12 (count rectangles))
        buffer ^java.nio.FloatBuffer (native-buffer/ensure-buffer-capacity buffer  number-of-coordinates)]
    

    (doseq [rectangle rectangles]
      (let [{:keys [x y width height]} rectangle]
        (.put buffer #^floats (float-array [x y
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

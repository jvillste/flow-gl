(ns flow-gl.opengl.jogl.triangle-list
  (:require [flow-gl.opengl.jogl.buffer :as buffer]
            [flow-gl.opengl.jogl.shader :as shader]
            [flow-gl.opengl.jogl.vertex-array-object :as vertex-array-object]
            [flow-gl.opengl.math :as math])
  (:import (com.jogamp.opengl GL2)))

(defrecord TriangleList [mode
                         number-of-triangles
                         shader-program
                         vertex-coordinate-attribute-index
                         vertex-coordinate-buffer-id])

  (def single-color-vertex-shader-source "
  #version 140
  uniform mat4 projection_matrix;

  in vec2 vertex_coordinate_attribute;

  void main() {
  gl_Position = projection_matrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);

  }

")

  (def single-color-fragment-shader-source "
  #version 140

  uniform vec4 color;

  out vec4 outColor;

  void main() {
  outColor = color;
  }
")


#_(defn update-coordinates [triangle-list coordinates gl]
    (buffer/load-vertex-array-buffer gl
                                     (:vertex-coordinate-buffer-id triangle-list)
                                     :float
                                     coordinates)

    (assoc triangle-list
      :number-of-triangles (/ (count coordinates)
                              2
                              3)))

(defn update-coordinates-from-native-buffer [triangle-list native-buffer gl]
  (buffer/load-vertex-array-buffer gl
                                   (:vertex-coordinate-buffer-id triangle-list)
                                   native-buffer)

  (assoc triangle-list
    :number-of-triangles (/ (.limit native-buffer)
                            2
                            3)))

(defn initialize-vertex-array-object [triangle-list gl]
  (vertex-array-object/bind gl (:vertex-array-object triangle-list))
  (.glBindBuffer gl GL2/GL_ARRAY_BUFFER (:vertex-coordinate-buffer-id triangle-list))
  (.glVertexAttribPointer gl
                          (int (:vertex-coordinate-attribute-index triangle-list))
                          (int 2)
                          (int GL2/GL_FLOAT)
                          (boolean GL2/GL_FALSE)
                          (int 0)
                          (long 0))
  (.glEnableVertexAttribArray gl (:vertex-coordinate-attribute-index triangle-list))

  (vertex-array-object/bind gl 0)

  triangle-list)

(defn create [gl mode]
  (let [shader-program (shader/compile-program gl
                                               single-color-vertex-shader-source
                                               single-color-fragment-shader-source)]
    (-> (map->TriangleList {:mode mode
                            :vertex-array-object (vertex-array-object/create gl)
                            :vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")
                            :vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)
                            :shader-program shader-program})

        (initialize-vertex-array-object gl))))

(defn delete [triangle-list gl]
  (buffer/delete gl (:vertex-coordinate-buffer-id triangle-list))
  (shader/delete-program gl (:shader-program triangle-list))
  triangle-list)

(defn set-size [triangle-list width height gl]
  (shader/enable-program gl (:shader-program triangle-list))
  (shader/set-float4-matrix-uniform gl
                                    (:shader-program triangle-list)
                                    "projection_matrix"
                                    (math/core-matrix-to-opengl-matrix (math/projection-matrix-2d width
                                                                                                  height))))

(defn render-single-color [triangle-list color gl]
  (shader/enable-program gl (:shader-program triangle-list))

  (vertex-array-object/bind gl (:vertex-array-object triangle-list))

  (apply shader/set-float4-uniform gl
         (:shader-program triangle-list)
         "color"
         color)

  (case (:mode triangle-list)
    :triangles (.glDrawArrays gl GL2/GL_TRIANGLES 0 (* 3 (:number-of-triangles triangle-list)))
    :triangle-strip (.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 (+ 2 (:number-of-triangles triangle-list)))
    :triangle-fan (.glDrawArrays gl GL2/GL_TRIANGLE_FAN 0 (:number-of-triangles triangle-list)))

  (vertex-array-object/bind gl 0)

  triangle-list)

#_(defn render-coordinates [triangle-list coordinates color gl]
    (-> triangle-list
        (update-coordinates coordinates gl)
        (render-single-color color gl)))

(defn render-coordinates-from-native-buffer [triangle-list native-buffer color gl]
  (-> triangle-list
      (update-coordinates-from-native-buffer native-buffer gl)
      (render-single-color color gl)))

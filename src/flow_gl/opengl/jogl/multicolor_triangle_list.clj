(ns flow-gl.opengl.jogl.multicolor-triangle-list
  (:require (flow-gl.opengl.jogl [shader :as shader]
                                 [buffer :as buffer]
                                 [opengl :as opengl]
                                 [vertex-array-object :as vertex-array-object])
            [flow-gl.opengl.math :as math]
            [flow-gl.graphics.native-buffer :as native-buffer])
  (:import [javax.media.opengl GL2]
           [com.jogamp.common.nio Buffers]))

(defrecord TriangleList [mode
                         number-of-triangles
                         shader-program
                         vertex-coordinate-attribute-index
                         ertex-color-attribute-index
                         vertex-coordinate-buffer-id
                         vertex-coordinate-buffer
                         vertex-color-buffer-id
                         vertex-color-buffer])

  (def vertex-shader-source "
  #version 140
  uniform mat4 projection_matrix;

  in vec2 vertex_coordinate_attribute;
  in vec4 vertex_color_attribute;

  out vec4 color;

  void main() {
  gl_Position = projection_matrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
  color = vertex_color_attribute;
  }

")


  (def fragment-shader-source "
  #version 140

  in vec4 color;
  out vec4 outColor;

  void main() {
  outColor = color;
  }
")



(defn update-coordinates [triangle-list coordinates gl]
  (let [float-buffer (native-buffer/ensure-buffer-capacity-with-values (:float-buffer triangle-list)
                                                                       coordinates)]
    (buffer/load-vertex-array-buffer gl
                                     (:vertex-coordinate-buffer-id triangle-list)
                                     float-buffer)

    (assoc triangle-list
      :float-buffer float-buffer
      :number-of-triangles (/ (count coordinates)
                              2
                              3))))

(defn update [triangle-list coordinates colors gl]
  (let [float-buffer (native-buffer/ensure-buffer-capacity-with-values (:float-buffer triangle-list)
                                                                       colors)]

    (buffer/load-vertex-array-buffer gl
                                     (:vertex-color-buffer-id triangle-list)
                                     float-buffer)

    (-> triangle-list
        (assoc :float-buffer float-buffer)
        (update-coordinates coordinates gl))))

(defn initialize-vertex-array-object [triangle-list gl]
  (vertex-array-object/bind gl (:vertex-array-object triangle-list))
  (.glBindBuffer gl GL2/GL_ARRAY_BUFFER (:vertex-coordinate-buffer-id triangle-list))
  (.glVertexAttribPointer gl
                          (int (:vertex-coordinate-attribute-index triangle-list))
                          (int 2)
                          (int GL2/GL_FLOAT)
                          false
                          (int 0)
                          (long 0))
  (.glEnableVertexAttribArray gl (:vertex-coordinate-attribute-index triangle-list))

  (.glBindBuffer gl GL2/GL_ARRAY_BUFFER (:vertex-color-buffer-id triangle-list))
  (.glVertexAttribPointer gl
                          (int (:vertex-color-attribute-index triangle-list))
                          (int 4)
                          (int GL2/GL_FLOAT)
                          false
                          (int 0)
                          (long 0))
  (.glEnableVertexAttribArray gl (:vertex-color-attribute-index triangle-list))

  (vertex-array-object/bind gl 0)

  triangle-list)

(defn create [gl mode]
  (let [shader-program (shader/compile-program gl
                                               vertex-shader-source
                                               fragment-shader-source)
        vertex-array-object (vertex-array-object/create gl)]

    (vertex-array-object/bind gl vertex-array-object)


    (-> (map->TriangleList {:mode mode
                            :vertex-array-object (vertex-array-object/create gl)
                            :float-buffer (native-buffer/create-native-buffer :float 256)
                            :vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")
                            :vertex-color-attribute-index (.glGetAttribLocation gl shader-program "vertex_color_attribute")
                            :vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)
                            :vertex-color-buffer-id (buffer/create-gl-buffer gl)
                            :shader-program shader-program})

        (initialize-vertex-array-object gl))))

(defn create-for-coordinates [gl mode coordinates colors]
  (-> (create gl mode)
      (update gl
              coordinates
              colors)))

(defn delete [triangle-list gl]
  (buffer/delete gl (:vertex-coordinate-buffer-id triangle-list))
  (buffer/delete gl (:vertex-color-buffer-id triangle-list))
  (shader/delete-program gl (:shader-program triangle-list))
  triangle-list)

(defn set-size [triangle-list width height gl]
  (shader/enable-program gl (:shader-program triangle-list))
  (shader/set-float4-matrix-uniform gl
                                    (:shader-program triangle-list)
                                    "projection_matrix"
                                    (math/core-matrix-to-opengl-matrix (math/projection-matrix-2d width
                                                                                                  height)))
  triangle-list)

(defn render [triangle-list gl]
  (vertex-array-object/bind gl (:vertex-array-object triangle-list))
  (shader/enable-program gl (:shader-program triangle-list))

  (case (:mode triangle-list)
    :triangles (.glDrawArrays gl GL2/GL_TRIANGLES 0 (* 3 (:number-of-triangles triangle-list)))
    :triangle-strip (.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 (+ 2 (:number-of-triangles triangle-list)))
    :triangle-fan (.glDrawArrays gl GL2/GL_TRIANGLE_FAN 0 (:number-of-triangles triangle-list)))

  (vertex-array-object/bind gl 0)

  triangle-list)



(defn render-coordinates [triangle-list coordinates colors gl]
  (-> triangle-list
      (update coordinates colors gl)
      (render gl)))

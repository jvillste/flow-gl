(ns flow-gl.opengl.jogl.triangle-list
  (:require (flow-gl.opengl.jogl [shader :as shader]
                                 [buffer :as buffer]
                                 [opengl :as opengl])
            [flow-gl.opengl.math :as math])
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


(defn update-coordinates [triangle-list coordinates gl]
  (buffer/load-vertex-array-buffer gl
                                   (:vertex-coordinate-buffer-id triangle-list)
                                   :float
                                   coordinates)

  (assoc triangle-list
    :number-of-triangles (/ (count coordinates)
                            2
                            3)))

(defn update [triangle-list coordinates colors gl]

  (buffer/load-vertex-array-buffer gl
                                   (:vertex-color-buffer-id triangle-list)
                                   :float
                                   colors)

  (update-coordinates triangle-list coordinates gl))




(def shader-program-atom (atom nil))
(def single-color-shader-program-atom (atom nil))

(defn create-shared-resources [gl]
  (reset! shader-program-atom (shader/compile-program gl
                                                      vertex-shader-source
                                                      fragment-shader-source))

  (reset! single-color-shader-program-atom (shader/compile-program gl
                                                                   single-color-vertex-shader-source
                                                                   single-color-fragment-shader-source)))

(defn delete-shared-resources [gl]
  (shader/delete-program gl @shader-program-atom)
  (shader/delete-program gl @single-color-shader-program-atom)
  (reset! shader-program-atom nil)
  (reset! single-color-shader-program-atom nil))

(defn create [gl mode]
  (map->TriangleList {:mode mode
                      :vertex-coordinate-attribute-index (.glGetAttribLocation gl @shader-program-atom "vertex_coordinate_attribute")
                      :vertex-color-attribute-index (.glGetAttribLocation gl @shader-program-atom "vertex_color_attribute")
                      :vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)
                      :vertex-color-buffer-id (buffer/create-gl-buffer gl)}))

(defn create-for-coordinates [gl mode coordinates colors]
  (-> (create gl mode)
      (update gl
              coordinates
              colors)))

(defn delete [triangle-list gl]
  (buffer/delete gl (:vertex-coordinate-buffer-id triangle-list))
  (buffer/delete gl (:vertex-color-buffer-id triangle-list))
  triangle-list)

(defn set-size [triangle-list width height gl]
  (shader/enable-program gl @single-color-shader-program-atom)
  (shader/set-float4-matrix-uniform gl
                                    @single-color-shader-program-atom
                                    "projection_matrix"
                                    (math/projection-matrix-2d width
                                                               height))

  (shader/enable-program gl @shader-program-atom)
  (shader/set-float4-matrix-uniform gl
                                    @shader-program-atom
                                    "projection_matrix"
                                    (math/projection-matrix-2d width
                                                               height)))

(defn render [triangle-list gl]
  (shader/enable-program gl @shader-program-atom)

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

  (case (:mode triangle-list)
    :triangles (.glDrawArrays gl GL2/GL_TRIANGLES 0 (* 3 (:number-of-triangles triangle-list)))
    :triangle-strip (.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 (+ 2 (:number-of-triangles triangle-list)))
    :triangle-fan (.glDrawArrays gl GL2/GL_TRIANGLE_FAN 0 (:number-of-triangles triangle-list)))

  (.glDisableVertexAttribArray gl (:vertex-coordinate-attribute-index triangle-list))
  (.glDisableVertexAttribArray gl (:vertex-color-attribute-index triangle-list))

  triangle-list)



(defn render-single-color [triangle-list color gl]
  (shader/enable-program gl @single-color-shader-program-atom)

  (apply shader/set-float4-uniform gl
         @single-color-shader-program-atom
         "color"
         color)

  (.glBindBuffer gl GL2/GL_ARRAY_BUFFER (:vertex-coordinate-buffer-id triangle-list))
  (.glVertexAttribPointer gl
                          (int (:vertex-coordinate-attribute-index triangle-list))
                          (int 2)
                          (int GL2/GL_FLOAT)
                          (boolean GL2/GL_FALSE)
                          (int 0)
                          (long 0))
  (.glEnableVertexAttribArray gl (:vertex-coordinate-attribute-index triangle-list))

  (case (:mode triangle-list)
    :triangles (.glDrawArrays gl GL2/GL_TRIANGLES 0 (* 3 (:number-of-triangles triangle-list)))
    :triangle-strip (.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 (+ 2 (:number-of-triangles triangle-list)))
    :triangle-fan (.glDrawArrays gl GL2/GL_TRIANGLE_FAN 0 (:number-of-triangles triangle-list)))

  (.glDisableVertexAttribArray gl (:vertex-coordinate-attribute-index triangle-list))

  triangle-list)

(defn render-coordinates [triangle-list coordinates colors gl]
  (-> triangle-list
      (update coordinates colors gl)
      (render gl)))

(defn render-single-color-coordinates [triangle-list coordinates color gl]
  (-> triangle-list
      (update-coordinates coordinates gl)
      (render-single-color color gl)))

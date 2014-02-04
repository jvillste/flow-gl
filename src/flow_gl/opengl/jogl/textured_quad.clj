(ns flow-gl.opengl.jogl.textured-quad
  (:require (flow-gl.opengl.jogl [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer]))
  (:import [javax.media.opengl GL2]))


(def vertex-shader-source "
#version 120

attribute vec2 vertex_coordinate_attribute;
attribute vec2 texture_coordinate_attribute;

varying vec2 texture_coordinate;

void main() {
    gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
    texture_coordinate = texture_coordinate_attribute;
}

")

(def fragment-shader-source "
#version 120

uniform sampler2D texture;
varying vec2 texture_coordinate;

void main() {
    gl_FragColor = texture2D(texture, texture_coordinate);
}
")

(def shared-resources-atom (atom nil))

(defn create-shared-resources [gl]
  (let [shader-program (shader/compile-program gl
                                               vertex-shader-source
                                               fragment-shader-source)
        vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")
        texture-coordinate-attribute-index (.glGetAttribLocation gl shader-program "texture_coordinate_attribute")

        texture-coordinate-buffer-id (buffer/create-gl-buffer gl)]

    (buffer/load-buffer gl
                        texture-coordinate-buffer-id
                        :float
                        (map float [1 1
                                    1 0
                                    0 0
                                    0 1]))

    (reset! shared-resources-atom {:shader-program shader-program
                                   :texture-coordinate-buffer-id texture-coordinate-buffer-id
                                   :vertex-coordinate-attribute-index vertex-coordinate-attribute-index
                                   :texture-coordinate-attribute-index texture-coordinate-attribute-index})))

(defn delete-shared-resources [gl]
  (shader/delete-program gl (:shader-program @shared-resources-atom))
  (buffer/delete gl (:texture-coordinate-buffer-id @shared-resources-atom))
  (reset! shared-resources-atom nil))

(defn quad [width height]
  [width height
   width 0
   0   0
   0   height])

(defn width [textured-quad]
  (:width (:texture textured-quad)))

(defn height [textured-quad]
  (:height (:texture textured-quad)))

(defn update-vertexes [textured-quad gl]
  (buffer/load-buffer gl
                      (:vertex-coordinate-buffer-id textured-quad)
                      :float
                      (map float (quad (width textured-quad)
                                       (height textured-quad))))
  textured-quad)


(defn create [texture gl]
  (-> {:texture texture
       :vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)}
      (update-vertexes gl)))

(defn delete [textured-quad gl]
  (texture/delete (:texture textured-quad) gl)
  (buffer/delete gl (:vertex-coordinate-buffer-id textured-quad)))

(defn render [textured-quad gl]
  (shader/enable-program gl
                         (:shader-program @shared-resources-atom))

  (texture/bind (:texture textured-quad)
                gl)

  (buffer/bind-buffer gl (:vertex-coordinate-buffer-id textured-quad))
  (.glEnableVertexAttribArray gl (:vertex-coordinate-attribute-index @shared-resources-atom))
  (.glVertexAttribPointer gl
                          (int (:vertex-coordinate-attribute-index @shared-resources-atom))
                          (int 2)
                          (int GL2/GL_FLOAT)
                          (boolean GL2/GL_FALSE)
                          (int 0)
                          (long 0))

  (buffer/bind-buffer gl (:texture-coordinate-buffer-id @shared-resources-atom))
  (.glEnableVertexAttribArray gl
                              (:texture-coordinate-attribute-index @shared-resources-atom))
  (.glVertexAttribPointer gl
                          (int (:texture-coordinate-attribute-index @shared-resources-atom))
                          (int 2)
                          (int GL2/GL_FLOAT)
                          (boolean GL2/GL_FALSE)
                          (int 0)
                          (long 0))

  (.glDrawArrays gl GL2/GL_QUADS 0 4)

  textured-quad)

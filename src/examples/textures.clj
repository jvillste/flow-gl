(ns examples.textures
  (:require [flow-gl.gui.event-queue :as event-queue]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer])
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]))

  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]))


(def vertex-shader-source "
#version 140

in vec2 vertex_coordinate_attribute;

attribute vec1 texture_offset_attribute;
in uint texture_width_attribute;
in uint texture_height_attribute;

out vec2 texture_coordinate;
out uint texture_offset;
out uint texture_width;
out uint texture_height;

void main() {
    gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
    texture_coordinate = texture_coordinate_attribute;
    texture_offset = texture_offset_attribute;
    texture_width = texture_width_attribute;
    texture_height = texture_height_attribute;
}

")

(def fragment-shader-source "
#version 140

uniform samplerBuffer texture;
// uniform sampler2D texture;
in vec2 texture_coordinate;
in uint texture_offset;
in uint texture_width;
in uint texture_height;

void main() {
    gl_FragColor = texelFetch(texture, texture_offset + texture_coordinate.x * texture_width + texture_coordinate.y * texture_height);
}
")

(defn create-gl-texture [gl]
  (let [result (int-array 1)]
    (.glGenTextures gl 1 result 0)
    (first result)))

(defn bind-texture-buffer [gl id]
  (.glBindTexture gl GL2/GL_TEXTURE_BUFFER id))

#_(defn create-int-buffer [buffered-image]
    (let [bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))
          byte-buffer (IntBuffer/allocateDirect (alength bytes))]
                                        ;(.order byte-buffer (ByteOrder/nativeOrder))
      (.put byte-buffer bytes 0 (alength bytes))
      (.flip byte-buffer)))

(defn load-attribute-array [name program gl type size divisor values]
  (let [attribute-location (.glGetAttribLocation gl program name)
        buffer-id (buffer/create-gl-buffer gl)]

    (buffer/load-buffer gl
                        buffer-id
                        type
                        values)
    (buffer/bind-buffer gl buffer-id)
    (.glEnableVertexAttribArray gl attribute-location)
    (.glVertexAttribPointer gl
                            (int attribute-location)
                            (int size)
                            (case type
                              :float GL2/GL_FLOAT
                              :int GL2/GL_INT)
                            false
                            (int 0)
                            (long 0))
    (.glVertexAttribDivisor attribute-location
                            divisor)))

(defn start []
  (let [width 300
        height 300
        window (window/create width height :profile :gl3)]

    (try
      (window/render window gl
                     (println (.glGetString gl GL2/GL_SHADING_LANGUAGE_VERSION))
                     (opengl/initialize gl)
                     (opengl/resize gl width height)

                     (let [shader-program (shader/compile-program gl
                                                                  vertex-shader-source
                                                                  fragment-shader-source)

                           texture-buffer-id (buffer/create-gl-buffer gl)
                           buffered-image (buffered-image/create-from-file "pumpkin.png")


                           texture-id (create-gl-texture gl)]

                       (load-attribute-array "vertex_coordinate_attribute" shader-program gl :float 2 0 (textured-quad/quad 20 20))
                       (load-attribute-array "texture_coordinate_attribute" shader-program gl :float 2 0 (textured-quad/quad 1 1))
                       (load-attribute-array "texture_offset_attribute" shader-program gl :int 1 1 [0])
                       (load-attribute-array "texture_width_attribute" shader-program gl :int 1 1 [(.getWidth buffered-image)])
                       (load-attribute-array "texture_height_attribute" shader-program gl :int 1 1 [(.getHeight buffered-image)])


                       (println "binding")
                       (.glBindBuffer gl GL2/GL_TEXTURE_BUFFER texture-buffer-id)
                       (println "bound")

                       (let [image-bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))]
                         (.glBufferData gl
                                        GL2/GL_TEXTURE_BUFFER
                                        (.remaining image-bytes)
                                        image-bytes
                                        GL2/GL_STATIC_DRAW))
                       (.glBindTexture gl GL2/GL_TEXTURE_BUFFER texture-id)
                       (.glTexBuffer gl GL2/GL_RGBA8UI texture-buffer-id)

                       (shader/enable-program gl
                                              shader-program)

                       (.glDrawArraysInstanced gl GL2/GL_QUADS 0 4 1)))

      (catch Exception e
        (window/close window)
        (throw e)))))

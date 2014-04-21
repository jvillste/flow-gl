(ns examples.opengl
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

uniform samplerBuffer texture;
uniform samplerBuffer texture_offset;

in vec2 vertex_coordinate_attribute;

in vec2 texture_coordinate_attribute;
in uint texture_offset_attribute;
in uint texture_width_attribute;

out vec2 texture_coordinate;
out uint texture_offset;
out uint texture_width;

void main() {
    gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
    texture_coordinate = texture_coordinate_attribute;
    texture_offset = texture_offset_attribute;
    texture_width = texture_width_attribute;
}

")

(def fragment-shader-source "
#version 140

uniform samplerBuffer texture;
// uniform sampler2D texture;
in vec2 texture_coordinate;
in uint texture_offset;
in uint texture_width;


void main() {
    gl_FragColor = texelFetch(texture, texture_coordinate);
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


(defn start []
  (let [width 300
        height 300
        window (window/create width height)]

    (try
      (window/render window gl
                     (opengl/initialize gl)
                     (opengl/resize gl width height)

                     (let [shader-program (shader/compile-program gl
                                                                  vertex-shader-source
                                                                  fragment-shader-source)
                           vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")

                           texture-coordinate-attribute-index (.glGetAttribLocation gl shader-program "texture_coordinate_attribute")

                           vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)

                           texture-buffer-id (buffer/create-gl-buffer gl)
                           buffered-image (buffered-image/create-from-file "pumpkin.png")
                           image-bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))

                           texture-id (create-gl-texture gl)]

                       (buffer/load-buffer gl
                                           vertex-coordinate-buffer-id
                                           :float
                                           (map float (textured-quad/quad 20 20)))

                       (println "binding")
                       (.glBindBuffer gl GL2/GL_TEXTURE_BUFFER texture-buffer-id)
                       (println "bound")
                       (.glBufferData gl
                                      GL2/GL_TEXTURE_BUFFER
                                      (.remaining image-bytes)
                                      image-bytes
                                      GL2/GL_STATIC_DRAW)

                       (.glBindTexture gl GL2/GL_TEXTURE_BUFFER texture-id)
                       (.glTexBuffer gl GL2/GL_RGBA8UI texture-buffer-id)

                       (shader/enable-program gl
                                              shader-program)

                       (buffer/bind-buffer gl vertex-coordinate-buffer-id)
                       (.glEnableVertexAttribArray gl vertex-coordinate-attribute-index)
                       (.glVertexAttribPointer gl
                                               (int vertex-coordinate-attribute-index)
                                               (int 2)
                                               (int GL2/GL_FLOAT)
                                               (boolean GL2/GL_FALSE)
                                               (int 0)
                                               (long 0))

                       (.glDrawArrays gl GL2/GL_QUADS 0 4)))

      (catch Exception e
        (window/close window)
        (throw e)))))

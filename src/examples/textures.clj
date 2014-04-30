(ns examples.textures
  (:require [flow-gl.gui.event-queue :as event-queue]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer])
            [flow-gl.opengl.math :as math]
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]
                              [native-buffer :as native-buffer]))

  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color]))


(def vertex-shader-source "
#version 140

uniform mat4 projection_matrix;

in vec2 vertex_coordinate_attribute;
//in vec2 texture_coordinate_attribute;

const vec2 texture_coordinates[] = vec2[4](
  vec2(0.0,  0.0),
  vec2(0.0, 1.0),
  vec2(1.0,   0.0),
  vec2(1.0,  1.0)
);

in int texture_offset_attribute;
in int texture_width_attribute;
in int texture_height_attribute;

out vec2 texture_coordinate;
flat out int texture_offset;
flat out int texture_width;
flat out int texture_height;

void main() {
    gl_Position = projection_matrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
    texture_coordinate = texture_coordinates[gl_VertexID];
    texture_offset = texture_offset_attribute;
    texture_width = texture_width_attribute;
    texture_height = texture_height_attribute;
}

")

(def fragment-shader-source "
#version 140

uniform samplerBuffer texture;

in vec2 texture_coordinate;
flat in int texture_offset;
flat in int texture_width;
flat in int texture_height;

out vec4 outColor;

void main() {
    //float value = texture_coordinate.x;
    //if(int(texture_width) == 127)

    //float value = texelFetch(texture, 100).r;
    //float value = 0.2;
    //outColor = vec4(value, value, value, 1.0);
    //outColor = texelFetch(texture, int(texture_offset + texture_coordinate.x * texture_width + texture_coordinate.y * texture_height));
    //outColor = texelFetch(texture, int(texture_offset + texture_coordinate.x * texture_width + texture_coordinate.y * texture_height));
    vec4 color = texelFetch(texture, int(texture_offset +  int(texture_coordinate.y * texture_height) * texture_width + texture_coordinate.x * texture_width));
    outColor = vec4(color.b, color.g, color.r, color.a);
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

    (case type
      :float (.glVertexAttribPointer gl
                            (int attribute-location)
                            (int size)
                            GL2/GL_FLOAT
                            false
                            (int 0)
                            (long 0))

      :int (.glVertexAttribIPointer gl
                            (int attribute-location)
                            (int size)
                            GL2/GL_INT
                            (int 0)
                            (long 0)))
    
    (.glVertexAttribDivisor gl
                            attribute-location
                            divisor)))

(defn buffere-image-data-to-rgba-native-buffer [int-values]
  (let [native-buffer (native-buffer/native-buffer :byte (* 4 (count int-values)))]
    (.rewind native-buffer)
    (doseq [value int-values]
      ;;(println "put " value " " (extract-byte value 2) " " (extract-byte value 1) " " (extract-byte value 0) " " (extract-byte value 3))
      (.put native-buffer (unchecked-byte (bit-and (bit-shift-right value 16) 0xff)))
      (.put native-buffer (unchecked-byte (bit-and (bit-shift-right value 8) 0xff)))
      (.put native-buffer (unchecked-byte (bit-and value 0xff)))
      (.put native-buffer (unchecked-byte (bit-and (bit-shift-right value  24) 0xff))))
    (.rewind native-buffer)
    native-buffer))


(defn create-buffered-image []
  #_(let [rectangle-width 128
          rectangle-height 128
          buffered-image (buffered-image/create rectangle-width rectangle-height)
          graphics (buffered-image/get-graphics buffered-image)]
      (.setColor graphics (Color. 254 0 0 254))
      (.fillRect graphics 0 0 rectangle-width rectangle-height)
      buffered-image)
  (text/create-buffered-image [0 0 1 1]
                              (font/create "LiberationSans-Regular.ttf" 20)
                              "Hello World!")
  #_(buffered-image/create-from-file "pumpkin.png"))

(defn create-image-bank []
  {:coordinates []
   })

(defn add-image [image-bank]
  )

(defn start []
  (let [width 300
        height 300
        window (window/create width height :profile :gl3)]

    (try
      (window/render window gl
                     (opengl/initialize gl)
                     (opengl/resize gl width height)

                     (let [shader-program (shader/compile-program gl
                                                                  vertex-shader-source
                                                                  fragment-shader-source)

                           texture-buffer-id (buffer/create-gl-buffer gl)
                           buffered-image (create-buffered-image)

                           ;;image-native-buffer (buffere-image-data-to-rgba-native-buffer (-> buffered-image (.getRaster) (.getDataBuffer) (.getData)))
                           image-native-buffer (native-buffer/native-buffer-with-values :int (-> buffered-image (.getRaster) (.getDataBuffer) (.getData)))
                           #_image-native-buffer #_(native-buffer/native-buffer-with-values :byte [255 255 255 255
                                                                                                   0 255 255 255])

                           texture-id (create-gl-texture gl)]


                       (load-attribute-array "vertex_coordinate_attribute" shader-program gl :float 2 0 (textured-quad/quad (.getWidth buffered-image)
                                                                                                                            (.getHeight buffered-image)))
                       ;;(load-attribute-array "texture_coordinate_attribute" shader-program gl :float 2 0 (textured-quad/quad 1 1))
                       (load-attribute-array "texture_offset_attribute" shader-program gl :int 1 1 [0])
                       (load-attribute-array "texture_width_attribute" shader-program gl :int 1 1 [(.getWidth buffered-image)])
                       (load-attribute-array "texture_height_attribute" shader-program gl :int 1 1 [(.getHeight buffered-image)])

                       (.glBindBuffer gl GL2/GL_TEXTURE_BUFFER texture-buffer-id)
                       (.glBufferData gl
                                      GL2/GL_TEXTURE_BUFFER
                                      (* 4 (.getWidth buffered-image) (.getHeight buffered-image))
                                      image-native-buffer
                                      GL2/GL_STATIC_DRAW)
                       (.glBindBuffer gl GL2/GL_TEXTURE_BUFFER 0)



                       ;;Draw

                       (shader/enable-program gl
                                              shader-program)
                       (.glActiveTexture gl GL2/GL_TEXTURE0)
                       (.glBindTexture gl GL2/GL_TEXTURE_BUFFER texture-id)
                       ;;(.glTexBuffer gl GL2/GL_TEXTURE_BUFFER GL2/GL_RGBA8UI texture-buffer-id)
                       ;;(.glTexBuffer gl GL2/GL_TEXTURE_BUFFER GL2/GL_R8 texture-buffer-id)
                       (.glTexBuffer gl GL2/GL_TEXTURE_BUFFER GL2/GL_RGBA8 texture-buffer-id)

                       (shader/set-float4-matrix-uniform gl
                                                         shader-program
                                                         "projection_matrix"
                                                         (math/projection-matrix-2d width
                                                                                    height
                                                                                    1.0))
                       ;; (.glPrimitiveRestartIndex gl 4000)

                       (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 1)
                       ;;(.glDrawArrays gl GL2/GL_TRIANGLES 0 3)

                       ;;(.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 4)
                       ))

      (catch Exception e
        (window/close window)
        (throw e)))))

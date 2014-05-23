(ns examples.frame-buffers
  (:require [flow-gl.gui.event-queue :as event-queue]
            (flow-gl.opengl.jogl [frame-buffer :as frame-buffer]
                                 [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer]
                                 [quad-batch :as quad-batch])
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

out vec2 texture_coordinate;

void main() {
    gl_Position = projection_matrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);

    switch(gl_VertexID) {
      case 0:
          texture_coordinate = vec2(0.0, 0.0);
        break;
      case 1:
          texture_coordinate = vec2(0.0, 1.0);
        break;
      case 2:
          texture_coordinate = vec2(1.0, 0.0);
        break;
      case 3:
          texture_coordinate = vec2(1.0, 1.0);
        break;
    }

}

")

(def fragment-shader-source "
#version 140

uniform sampler2D texture;
in vec2 texture_coordinate;
out vec4 outColor;

void main() {
    outColor = texture(texture, texture_coordinate);
    //outColor = texture(texture, vec2(0.5, 0.5));
    //outColor = vec4(1.0, 0.0, 0.0, 1.0);
}
")


(defn text-image [text]
  (text/create-buffered-image [1 1 1 1]
                              (font/create "LiberationSans-Regular.ttf" 14)
                              text))

(defn quad [width height]
  [0   0
   0   height
   width 0
   width height])

(defn start []
  (let [width 500
        height 500
        window (window/create width height :profile :gl3)]

    (try
      (window/with-gl window gl
        (opengl/initialize gl)

        #_(-> (buffered-image/create-from-file "pumpkin.png")
              (texture/create-for-buffered-image gl)
              (textured-quad/create gl)
              (textured-quad/render gl 256 256))

        (let [texture-width 40
              texture-height 40
              texture (texture/create-gl-texture gl)
              frame-buffer (frame-buffer/create gl)
              image (buffered-image/create-from-file "pumpkin.png")
              pumpkin-texture (texture/create-for-buffered-image image gl)

              shader-program (shader/compile-program gl
                                                     vertex-shader-source
                                                     fragment-shader-source)
              vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")
              vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)]

          (opengl/clear gl 0 0 0 1)


          (.glBindTexture gl GL2/GL_TEXTURE_2D texture)

          (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
          (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)
          #_(.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA texture-width texture-height 0 GL2/GL_RED GL2/GL_FLOAT
                           (native-buffer/native-buffer-with-values :float (repeat (* texture-width texture-height 1) 1.0)))

          #_(.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA 2 1 0 GL2/GL_RGBA GL2/GL_FLOAT
                         (native-buffer/native-buffer-with-values :float [0.0 0.0 1.0 1.0
                                                                          1.0 0.0 1.0 1.0]))

          (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA (.getWidth image) (.getHeight image) 0 GL2/GL_BGRA GL2/GL_UNSIGNED_BYTE
                           (native-buffer/native-buffer-with-values :int (-> image (.getRaster) (.getDataBuffer) (.getData))))

          (shader/enable-program gl
                                 shader-program)

          (shader/set-int-uniform gl
                                  shader-program
                                  "texture"
                                  0)
          (.glActiveTexture gl GL2/GL_TEXTURE0)
          (.glBindTexture gl GL2/GL_TEXTURE_2D texture)

          (shader/set-float4-matrix-uniform gl
                                            shader-program
                                            "projection_matrix"
                                            (math/projection-matrix-2d width
                                                                       height))

          (buffer/load-vertex-array-buffer gl
                                           vertex-coordinate-buffer-id
                                           :float
                                           (map float (quad texture-width
                                                            texture-height)))

          (.glBindBuffer gl GL2/GL_ARRAY_BUFFER vertex-coordinate-buffer-id)
          (.glEnableVertexAttribArray gl vertex-coordinate-attribute-index)
          (.glVertexAttribPointer gl
                                  (int vertex-coordinate-attribute-index)
                                  (int 2)
                                  (int GL2/GL_FLOAT)
                                  (boolean GL2/GL_FALSE)
                                  (int 0)
                                  (long 0))

          (.glDrawArrays gl GL2/GL_TRIANGLE_STRIP 0 4)

          ;;(.glEnable gl GL2/GL_TEXTURE_2D)



          #_(frame-buffer/bind gl frame-buffer)
          #_(frame-buffer/bind-texture gl texture)
          #_(println "status ok" (= GL2/GL_FRAMEBUFFER_COMPLETE
                                    (.glCheckFramebufferStatus gl GL2/GL_FRAMEBUFFER)))

          #_(-> pumpkin-texture
                (textured-quad/create gl)
                (textured-quad/render gl texture-width texture-height))

          #_(opengl/clear gl 1 0 0 1)

          #_(frame-buffer/bind gl 0)

          #_(-> (buffered-image/create-from-file "pumpkin.png")
                (texture/create-for-buffered-image gl)
                (textured-quad/create gl)
                (textured-quad/render gl 256 256))

          #_(textured-quad/render-gl-texture gl texture-width texture-height texture width height)))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))

;; TODO
;; optimize updating the same quads constantly. generational GC?
;; share texture
;; group quads to tiles and draw given tiles only
;; load new quads asynchronously in small batches. Ready made byte buffers to be loaded to the GPU

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

(def single-color-vertex-shader-source "
#version 140
uniform mat4 projection_matrix;

in vec2 vertex_coordinate_attribute;
out vec3 color;

void main() {

    switch(gl_VertexID) {
      case 0:
          color = vec3(1.0, 0.0, 0.0);
        break;
      case 1:
          color = vec3(0.0, 1.0, 0.0);
        break;
      case 2:
          color = vec3(0.0, 0.0, 1.0);
        break;
      case 3:
          color = vec3(1.0, 1.0, 0.0);
        break;
    }


   // gl_Position = projection_matrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
   gl_Position = vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
}

")

(def single-color-fragment-shader-source "
#version 330

layout(location = 0) out vec4 outColor;

in vec3 color;

void main() {
    //outColor = vec4(0.0, 0.0, 1.0, 1.0);
    outColor = vec4(color[0], color[1], color[2], 1.0);
    //gl_FragColor.rgb = vec3(1.0, 0.0, 0.0);
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

(defn draw-quad [gl texture quad-width quad-height frame-buffer-width frame-buffer-height]
  (let [shader-program (shader/compile-program gl
                                               vertex-shader-source
                                               fragment-shader-source)
        vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")
        vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)]

    (shader/enable-program gl
                           shader-program)

    #_(.glBindFragDataLocation gl
                               shader-program
                               0
                               "outColor")
    (println "location " (.glGetFragDataLocation gl shader-program "outColor"))

    (.glBindTexture gl GL2/GL_TEXTURE_2D texture)

    (shader/set-float4-matrix-uniform gl
                                      shader-program
                                      "projection_matrix"
                                      (math/projection-matrix-2d frame-buffer-width
                                                                 frame-buffer-height))

    (buffer/load-vertex-array-buffer gl
                                     vertex-coordinate-buffer-id
                                     :float
                                     (map float (quad quad-width
                                                      quad-height)))

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
    

    (.glBindTexture gl GL2/GL_TEXTURE_2D 0)
    (buffer/delete gl vertex-coordinate-buffer-id)
    (shader/delete-program gl shader-program)))

(defn quad-2 [from to]
  [from   from
   from   to
   to from
   to to])

(defn draw-single-color-quad [gl quad-width quad-height frame-buffer-width frame-buffer-height]
  (let [shader-program (shader/compile-program gl
                                               single-color-vertex-shader-source
                                               single-color-fragment-shader-source)
        vertex-coordinate-attribute-index (.glGetAttribLocation gl shader-program "vertex_coordinate_attribute")
        vertex-coordinate-buffer-id (buffer/create-gl-buffer gl)]

    (shader/enable-program gl
                           shader-program)

    #_(.glBindFragDataLocation gl
                               shader-program
                               1
                               "outColor")
    (println "single outColor location " (.glGetFragDataLocation gl shader-program "outColor"))

    (shader/set-float4-matrix-uniform gl
                                      shader-program
                                      "projection_matrix"
                                      (math/projection-matrix-2d frame-buffer-width
                                                                 frame-buffer-height))

    #_(buffer/load-vertex-array-buffer gl
                                       vertex-coordinate-buffer-id
                                       :float
                                       (map float (quad quad-width
                                                        quad-height)))

    #_(buffer/load-vertex-array-buffer gl
                                       vertex-coordinate-buffer-id
                                       :float
                                       (map float [-0.5   -0.5
                                                   -0.5   0.5
                                                   0.5 -0.5
                                                   0.5 0.5]))

    #_(buffer/load-vertex-array-buffer gl
                                       vertex-coordinate-buffer-id
                                       :float
                                       (map float [-1   -1
                                                   -1   1
                                                   1 -1
                                                   1 1]))

    (buffer/load-vertex-array-buffer gl
                                     vertex-coordinate-buffer-id
                                     :float
                                     (map float (quad-2 -0.9 0.9)))



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

    (.glDisableVertexAttribArray gl vertex-coordinate-attribute-index)
    (buffer/delete gl vertex-coordinate-buffer-id)
    (shader/delete-program gl shader-program)))

(defn create-texture [gl]
  (let [texture (texture/create-gl-texture gl)]
    (.glBindTexture gl GL2/GL_TEXTURE_2D texture)

    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)

    texture))

(defn load-texture [gl texture width height data]
  (.glBindTexture gl GL2/GL_TEXTURE_2D texture)
  (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA8 width height 0 GL2/GL_BGRA GL2/GL_UNSIGNED_BYTE data)
  (.glBindTexture gl GL2/GL_TEXTURE_2D 0))

(defn load-texture-from-buffered-image [gl texture image]
  (load-texture gl texture  (.getWidth image) (.getHeight image)
                (native-buffer/native-buffer-with-values :int (-> image (.getRaster) (.getDataBuffer) (.getData)))))

(defn start []
  (let [window-width 600
        window-height 600
        window (window/create window-width window-height :profile :gl3)]

    (try
      (window/with-gl window gl
        (opengl/initialize gl)

        #_(-> (buffered-image/create-from-file "pumpkin.png")
              (texture/create-for-buffered-image gl)
              (textured-quad/create gl)
              (textured-quad/render gl 256 256))

        (let [texture (create-texture gl)
              ;;frame-buffer-texture (create-texture gl)
              frame-buffer (frame-buffer/create gl)
              frame-buffer-width 500
              frame-buffer-height 500
              image (buffered-image/create-from-file "pumpkin.png")

              shader-program (shader/compile-program gl
                                                     vertex-shader-source
                                                     fragment-shader-source)]
          ;;(frame-buffer/bind gl frame-buffer)
          (.glBindFramebuffer gl GL2/GL_FRAMEBUFFER frame-buffer)

          (load-texture-from-buffered-image gl texture image)

          ;;(.glDisable gl GL2/GL_DEPTH_TEST)
          ;;(.glDepthMask gl false)

          (let [frame-buffer-texture (texture/create-gl-texture gl)]
            ;;(.glActiveTexture gl GL2/GL_TEXTURE0)
            (.glBindTexture gl GL2/GL_TEXTURE_2D frame-buffer-texture)

            (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_LINEAR)
            (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_LINEAR)
            (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
            (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)

            ;;(load-texture gl frame-buffer-texture frame-buffer-width frame-buffer-height nil)
            (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA frame-buffer-width frame-buffer-height 0 GL2/GL_RGBA GL2/GL_UNSIGNED_BYTE nil)
            ;;            (.glBindTexture gl GL2/GL_TEXTURE_2D 0)


            ;;(frame-buffer/bind-texture gl frame-buffer-texture)
            (.glFramebufferTexture gl
                                   GL2/GL_FRAMEBUFFER
                                   GL2/GL_COLOR_ATTACHMENT0
                                   ;;                                   GL2/GL_TEXTURE_2D
                                   frame-buffer-texture
                                   0)

            (.glDrawBuffers gl 1 (int-array [GL2/GL_COLOR_ATTACHMENT0]) 0)

            ;;(.glDrawBuffer gl GL2/GL_COLOR_ATTACHMENT0)
            (println "status ok" (= GL2/GL_FRAMEBUFFER_COMPLETE (.glCheckFramebufferStatus gl GL2/GL_FRAMEBUFFER)))

            (.glBindFramebuffer gl GL2/GL_FRAMEBUFFER frame-buffer)

            ;;(.glViewport gl 0 0 frame-buffer-width frame-buffer-height)

            (opengl/clear gl 1 0 0 0.7)

            ;; (.glBegin gl GL2/GL_TRIANGLES)
            ;; (.glVertex2f gl 0 0)
            ;; (.glVertex2f gl 10 0)
            ;; (.glVertex2f gl 1 10)
            ;; (.glEnd gl)

            ;;(.glClear gl GL2/GL_COLOR_BUFFER_BIT)
            ;;(draw-quad gl texture (.getWidth image) (.getHeight image) frame-buffer-width frame-buffer-height)
            (draw-single-color-quad gl 10 10 frame-buffer-width frame-buffer-height)
            ;;(draw-quad gl texture (.getWidth image) (.getHeight image) window-width window-height)

            ;;(frame-buffer/bind gl 0)
            (.glBindFramebuffer gl GL2/GL_DRAW_FRAMEBUFFER 0)


            (opengl/clear gl 0 0 0 1)
            ;;(draw-quad gl texture (.getWidth image) (.getHeight image) window-width window-height)
            (draw-quad gl frame-buffer-texture frame-buffer-width frame-buffer-height window-width window-height)
            ;;(draw-single-color-quad gl 10 10 window-width window-height)
            )
          ))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))

;; TODO
;; optimize updating the same quads constantly. generational GC?
;; share texture
;; group quads to tiles and draw given tiles only
;; load new quads asynchronously in small batches. Ready made byte buffers to be loaded to the GPU

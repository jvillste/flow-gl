(ns flow-gl.opengl.jogl.render-target
  (:require [flow-gl.gui.event-queue :as event-queue]
            (flow-gl.opengl.jogl [frame-buffer :as frame-buffer]
                                 [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer]
                                 [quad-batch :as quad-batch]
                                 [vertex-array-object :as vertex-array-object]
                                 [render-buffer :as render-buffer])
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
  uniform vec4 quad_coordinates;

  in vec2 vertex_coordinate_attribute;

  out vec2 texture_coordinate;

  void main() {

  switch(gl_VertexID) {
  case 0:
  texture_coordinate = vec2(0.0, 1.0);
  break;

  case 1:
  texture_coordinate = vec2(0.0, 0.0);
  break;

  case 2:
  texture_coordinate = vec2(1.0, 1.0);
  break;

  case 3:
  texture_coordinate = vec2(1.0, 0.0);
  break;
  }

  gl_Position = projection_matrix * vec4(quad_coordinates[0] + quad_coordinates[2] * texture_coordinate.x,
  quad_coordinates[1] + quad_coordinates[3] * (1 - texture_coordinate.y),
  0.0, 1.0);

  }

")

  (def render-target-fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;

  out vec4 outColor;

  void main() {
  outColor = texture(texture, texture_coordinate);
  }
")

  (def fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;

  out vec4 outColor;

  void main() {
  outColor = texture(texture, vec2(texture_coordinate[0], 1 - texture_coordinate[1]));
  }
")

(defn draw-quad [gl textures fragment-shader-source x y quad-width quad-height frame-buffer-width frame-buffer-height]
  (let [shader-program (shader/compile-program gl
                                               vertex-shader-source
                                               fragment-shader-source)]

    (shader/enable-program gl
                           shader-program)

    (doall (map-indexed (fn [index [texture-name texture-id]]
                          (.glActiveTexture gl (+ index GL2/GL_TEXTURE0))
                          (.glBindTexture gl GL2/GL_TEXTURE_2D texture-id)
                          (.glUniform1i gl (.glGetUniformLocation gl shader-program texture-name) index))
                        textures))


    (shader/set-float4-matrix-uniform gl
                                      shader-program
                                      "projection_matrix"
                                      (math/projection-matrix-2d frame-buffer-width
                                                                 frame-buffer-height))

    (shader/set-float4-uniform gl
                               shader-program
                               "quad_coordinates"
                               x y quad-width quad-height)

    (let [vertex-array-object (vertex-array-object/create gl)]
      (vertex-array-object/bind gl vertex-array-object)

      (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 1)

      (vertex-array-object/bind gl 0)
      (vertex-array-object/delete gl vertex-array-object))

    (shader/delete-program gl shader-program)))

(defn create [width height gl]
  (let [frame-buffer (frame-buffer/create gl)
        frame-buffer-texture (texture/create gl)
        stencil-buffer (render-buffer/create-stencil-buffer width height gl)]

    (.glBindTexture gl GL2/GL_TEXTURE_2D frame-buffer-texture)

    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)

    (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 #_GL2/GL_RGBA32F GL2/GL_RGBA (int width) (int height) 0 GL2/GL_RGBA GL2/GL_UNSIGNED_BYTE nil)


    (frame-buffer/bind frame-buffer gl)
    (frame-buffer/bind-texture frame-buffer-texture gl)
    (frame-buffer/bind-stencil stencil-buffer gl)

    (.glDrawBuffers gl 1 (int-array [GL2/GL_COLOR_ATTACHMENT0]) 0)

    (assert (= (.glCheckFramebufferStatus gl GL2/GL_FRAMEBUFFER) GL2/GL_FRAMEBUFFER_COMPLETE))

    (frame-buffer/bind 0 gl)

    {:frame-buffer frame-buffer
     :texture frame-buffer-texture
     :width width
     :height height}))

(defn start-rendering [render-target gl]
  (frame-buffer/bind (:frame-buffer render-target)
                     gl)
  (.glViewport gl 0 0
               (:width render-target)
               (:height render-target)))


(defmacro render-to [render-target gl & body]
  `(do (let [size# (opengl/size ~gl)
             read# (frame-buffer/get-read ~gl)
             draw# (frame-buffer/get-draw ~gl)]
         (start-rendering ~render-target ~gl)
         (let [result# (do ~@body)]
           (frame-buffer/bind-read read# ~gl)
           (frame-buffer/bind-draw draw# ~gl)
           
           (.glViewport ~gl 0 0
                        (:width size#)
                        (:height size#))
           result#))))

(defn draw [render-target x y viewport-width viewport-height gl]
  (draw-quad gl
             [["texture" (:texture render-target)]]
             render-target-fragment-shader-source
             x
             y
             (:width render-target)
             (:height render-target)
             viewport-width
             viewport-height))

(defn delete [render-target gl]
  (frame-buffer/delete (:frame-buffer render-target) gl)
  (texture/delete gl (:texture render-target)))


#_(defn draw-rectangle [nanovg x y width height r g b a]
  (doto nanovg
    (NanoVG/fillColor (char r) (char g) (char b) (char a))
    (NanoVG/beginPath)
    (NanoVG/rect x y width height)
    (NanoVG/fill)))

(defn blit [render-target gl]
  (let [{:keys [width height]} (opengl/size gl)]
    (doto gl
      (.glBindFramebuffer GL2/GL_READ_FRAMEBUFFER (:frame-buffer render-target))
      (.glBindFramebuffer GL2/GL_DRAW_FRAMEBUFFER 0)
      (.glBlitFramebuffer 0 0 (:width render-target) (:height render-target)
                          0 (- height (:height render-target)) (:width render-target) height
                          GL2/GL_COLOR_BUFFER_BIT GL2/GL_LINEAR))))


#_(defn start []

    (let [window-width 600
          window-height 600
          window (window/create window-width
                                window-height
                                :profile :gl3
                                :close-automatically true
                                :init opengl/initialize
                                )]

      (try
        (window/set-display window gl


                            (let [{:keys [width height]} (opengl/size gl)
                                  nanovg (NanoVG/init)
                                  render-target (create 200 200
                                                        gl)
                                  texture (texture/create-for-file "pumpkin.png" gl)]


                              (render-to render-target gl
                                         (opengl/clear gl 0 1 1 1)
                                         (do
                                           (NanoVG/beginFrame nanovg (:width render-target) (:height render-target))
                                           (draw-rectangle nanovg
                                                           10 10 10 10
                                                           255 255 255 255)
                                           (NanoVG/endFrame nanovg))

                                         #_(draw-quad gl
                                                      [["texture" texture]]
                                                      fragment-shader-source
                                                      0 0
                                                      128 128
                                                      (:width render-target) (:height render-target)))

                              (opengl/clear gl 0 0 1 1)


                              (draw render-target 100 100 width height gl)

                              #_(do
                                  (NanoVG/beginFrame nanovg width height)
                                  (draw-rectangle nanovg
                                                  10 10 1000 1000
                                                  255 255 255 255)
                                  (NanoVG/endFrame nanovg))

                              (delete render-target gl)))

        (println "exiting")

        (catch Exception e
          (window/close window)
          (throw e)))))

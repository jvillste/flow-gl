(ns flow-gl.opengl.jogl.quad
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
                                 [vertex-array-object :as vertex-array-object])
            [flow-gl.opengl.math :as math]
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]
                              [native-buffer :as native-buffer]))

  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color]
           [nanovg NanoVG]))


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

  (def fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;

  out vec4 outColor;

  void main() {
  outColor = texture(texture, texture_coordinate);
  }
")

  (def upside-down-fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;

  out vec4 outColor;

  void main() {
  outColor = texture(texture, vec2(texture_coordinate[0], 1 - texture_coordinate[1]));
  }
")

(defn draw [gl textures fragment-shader-source x y quad-width quad-height frame-buffer-width frame-buffer-height]
  (println "quad draw" textures fragment-shader-source x y quad-width quad-height frame-buffer-width frame-buffer-height)
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

(defn start []

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
                                texture (texture/create-for-file "pumpkin.png" gl)]

                            (opengl/clear gl 0 0 1 1)

                            (draw gl
                                  [["texture" texture]]
                                  upside-down-fragment-shader-source
                                  0 0
                                  128 128
                                  width height)))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))

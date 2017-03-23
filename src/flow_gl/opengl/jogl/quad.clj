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

(def alpha-fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;

  uniform float alpha;

  out vec4 outColor;

  void main() {
  //vec4 texture_color = texture(texture, vec2(texture_coordinate[0], 1 - texture_coordinate[1]));
  vec4 texture_color = texture(texture, texture_coordinate);
  outColor = vec4(texture_color[0],texture_color[1],texture_color[2],texture_color[3] * alpha);
  }
  ")

(def blur-fragment-shader-source "
  // adopted from https://github.com/mattdesl/lwjgl-basics/wiki/ShaderLesson5

  #version 140
  
  in vec2 texture_coordinate;

  uniform sampler2D texture;
  uniform float resolution;
  uniform float radius;
  uniform vec2 dir;

  out vec4 outColor;

  void main() {

  //this will be our RGBA sum
  vec4 sum = vec4(0.0);

  //our original texcoord for this fragment
  vec2 tc = texture_coordinate;

  //the amount to blur, i.e. how far off center to sample from 
  //1.0 -> blur by one pixel
  //2.0 -> blur by two pixels, etc.
  float blur = radius/resolution; 

  //the direction of our blur
  //(1.0, 0.0) -> x-axis blur
  //(0.0, 1.0) -> y-axis blur
  float hstep = dir.x;
  float vstep = dir.y;

  //apply blurring, using a 9-tap filter with predefined gaussian weights

  sum += texture(texture, vec2(tc.x - 4.0*blur*hstep, tc.y - 4.0*blur*vstep)) * 0.0162162162;
  sum += texture(texture, vec2(tc.x - 3.0*blur*hstep, tc.y - 3.0*blur*vstep)) * 0.0540540541;
  sum += texture(texture, vec2(tc.x - 2.0*blur*hstep, tc.y - 2.0*blur*vstep)) * 0.1216216216;
  sum += texture(texture, vec2(tc.x - 1.0*blur*hstep, tc.y - 1.0*blur*vstep)) * 0.1945945946;

  sum += texture(texture, vec2(tc.x, tc.y)) * 0.2270270270;

  sum += texture(texture, vec2(tc.x + 1.0*blur*hstep, tc.y + 1.0*blur*vstep)) * 0.1945945946;
  sum += texture(texture, vec2(tc.x + 2.0*blur*hstep, tc.y + 2.0*blur*vstep)) * 0.1216216216;
  sum += texture(texture, vec2(tc.x + 3.0*blur*hstep, tc.y + 3.0*blur*vstep)) * 0.0540540541;
  sum += texture(texture, vec2(tc.x + 4.0*blur*hstep, tc.y + 4.0*blur*vstep)) * 0.0162162162;

  // outColor = vec4(1.0, 0.0, 0.0, 1.0) * 1.5 * vec4(sum.rgb, 1.0);
  outColor = 1.5 * vec4(sum.rgb, 1.0);
  // outColor = sum;
  }")

(defn create-program [fragment-shader-source gl]
  (let [fragment-shader (shader/create-fragment-shader gl fragment-shader-source)
        vertex-shader (shader/create-vertex-shader gl vertex-shader-source)
        program (shader/create-program gl
                                       vertex-shader
                                       fragment-shader)]
    (shader/delete-shader gl fragment-shader)
    (shader/delete-shader gl vertex-shader)
    program))

(defn program-stateful [fragment-shader-source gl]
  {:initialize-state (fn [] (create-program fragment-shader-source gl))
   :delete-state (fn [program] (shader/delete-program gl program))})

(defn draw [gl textures uniforms shader-program x y quad-width quad-height frame-buffer-width frame-buffer-height]
  (shader/enable-program gl
                         shader-program)

  (dorun (map-indexed (fn [index [texture-name texture-id]]
                        (.glActiveTexture gl (+ index GL2/GL_TEXTURE0))
                        (.glBindTexture gl GL2/GL_TEXTURE_2D texture-id)
                        (.glUniform1i gl (.glGetUniformLocation gl shader-program texture-name) index))
                      (partition 2 textures)))

  (doseq [[type name values] (partition 3 uniforms)]
    (let [location (.glGetUniformLocation gl shader-program name)]
      (case type
        :1i (.glUniform1i gl location values)
        :1f (.glUniform1f gl location values)
        :2f (let [[value1 value2] values]
              (.glUniform2f gl location value1 value2))
        :4f (let [[value1 value2 value3 value4] values]
              (.glUniform4f gl location value1 value2 value3 value4))
        :2fv (.glUniform2fv gl
                            location
                            (/ (count values) 2)
                            (float-array values)
                            0))))

  (shader/set-float4-matrix-uniform gl
                                    shader-program
                                    "projection_matrix"
                                    (math/core-matrix-to-opengl-matrix (math/projection-matrix-2d frame-buffer-width
                                                                                                  frame-buffer-height)))

  (shader/set-float4-uniform gl
                             shader-program
                             "quad_coordinates"
                             x y quad-width quad-height)

  (let [vertex-array-object (vertex-array-object/create gl)]
    (vertex-array-object/bind gl vertex-array-object)

    (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 1)

    (vertex-array-object/bind gl 0)
    (vertex-array-object/delete gl vertex-array-object))

  (shader/disable-program gl))

#_(defn start []

    (let [window-width 600
          window-height 600
          window (window/create window-width
                                window-height
                                :profile :gl3
                                :close-automatically true
                                :init opengl/initialize)]

      (try
        (window/set-display window gl
                            (let [quad (create gl)
                                  program (create-program quad alpha-fragment-shader-source gl)
                                  {:keys [width height]} (opengl/size gl)
                                  texture (texture/create-for-file "pumpkin.png" gl)]

                              (opengl/clear gl 0 0 1 1)

                              (draw gl
                                    ["texture" texture]
                                    [:1f "alpha" 0.5]
                                    program
                                    0 0
                                    128 128
                                    width height)

                              (draw gl
                                    ["texture" texture]
                                    [:1f "alpha" 1.0]
                                    program
                                    0 128
                                    228 228
                                    width height)

                              (shader/delete-program gl program)
                              (delete quad gl)))

        (println "exiting")

        (catch Exception e
          (window/close window)
          (throw e)))))

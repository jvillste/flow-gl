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
uniform samplerBuffer quad_coordinates_buffer;
uniform samplerBuffer parents;

in vec2 texture_size_attribute;
in int texture_offset_attribute;

out vec2 texture_coordinate;
flat out int texture_offset;
flat out float texture_width;

void main() {

    switch(gl_VertexID) {
      case 0:
          texture_coordinate = vec2(0.0, 0.0);
        break;
      case 1:
          texture_coordinate = vec2(0.0, texture_size_attribute.y);
        break;
      case 2:
          texture_coordinate = vec2(texture_size_attribute.x, 0.0);
        break;
      case 3:
          texture_coordinate = vec2(texture_size_attribute.x, texture_size_attribute.y);
        break;
    }

    int parent = int(texelFetch(parents, gl_InstanceID).x);
    vec2 offset = vec2(0,0);
    while(parent >= 0)
    {
       vec4 parent_offset = texelFetch(quad_coordinates_buffer, parent);
       offset = vec2(offset.x + parent_offset.x, offset.y + parent_offset.y);
       parent = int(texelFetch(parents, parent).x);
    }

    vec4 quad_coordinates = texelFetch(quad_coordinates_buffer, gl_InstanceID);
    gl_Position = projection_matrix * vec4(offset.x + texture_coordinate.x + quad_coordinates.x, offset.y +  texture_coordinate.y + quad_coordinates.y, 0.0, 1.0);

    texture_offset = texture_offset_attribute;
    texture_width = texture_size_attribute.x;
}
")

(def fragment-shader-source "
#version 140

uniform samplerBuffer texture;

in vec2 texture_coordinate;
flat  in int texture_offset;
flat in float texture_width;

out vec4 outColor;

void main() {
    vec4 color = texelFetch(texture, int(texture_offset + texture_coordinate.y * texture_width + texture_coordinate.x - texture_width / 2));
    outColor = vec4(color.b, color.g, color.r, color.a);
}
")

(defn create-vertex-attribute-array [gl name program buffer-id type size divisor]
  (let [attribute-location (.glGetAttribLocation gl program name)]
    (buffer/bind-buffer gl buffer-id)
    (.glEnableVertexAttribArray gl attribute-location)

    (if (= type :int)
      (.glVertexAttribIPointer gl
                               (int attribute-location)
                               (int size)
                               GL2/GL_INT
                               (int 0)
                               (long 0))

      (.glVertexAttribPointer gl
                              (int attribute-location)
                              (int size)
                              (case type
                                :float GL2/GL_FLOAT
                                :short GL2/GL_UNSIGNED_SHORT)
                              false
                              (int 0)
                              (long 0)))

    (.glVertexAttribDivisor gl
                            attribute-location
                            divisor)
    buffer-id))

(defn create-buffered-image []
  (let [rectangle-width 128
        rectangle-height 128
        buffered-image (buffered-image/create rectangle-width rectangle-height)
        graphics (buffered-image/get-graphics buffered-image)]
    (.setColor graphics (Color. 255 255 255 100))
    (.fillRect graphics 0 0 rectangle-width rectangle-height)
    buffered-image)
  #_(text/create-buffered-image [1 1 1 1]
                                (font/create "LiberationSans-Regular.ttf" 40)
                                "Hello World!")
  #_(buffered-image/create-from-file "pumpkin.png"))

(defn text-image [text]
  (text/create-buffered-image [1 1 1 1]
                              (font/create "LiberationSans-Regular.ttf" 40)
                              text))

(defn create-image-bank []
  (let [instance-capacity 100
        texture-capcacity (* instance-capacity 200 200)]
    {:coordinates (native-buffer/create-native-buffer :float (* 2 instance-capacity))
     :textrue-offsets (native-buffer/create-native-buffer :int (* 2 instance-capacity))
     :texture-sizes (native-buffer/create-native-buffer :int (* 2 instance-capacity))}))

(defn add-image [image-bank]
  )

(defn bind-texture-buffer [gl buffer-id texture-unit program uniform-name type]
  (shader/set-int-uniform gl
                          program
                          uniform-name
                          texture-unit)
  (.glActiveTexture gl (+ texture-unit GL2/GL_TEXTURE0))
  (.glBindTexture gl GL2/GL_TEXTURE_BUFFER (texture/create-gl-texture gl))
  (.glTexBuffer gl GL2/GL_TEXTURE_BUFFER type buffer-id))

(defn initialize-gpu [window]
  (let [gpu-state-atom (atom {})]
    (window/render window gl
                   (opengl/initialize gl)

                   (reset! gpu-state-atom
                           {:program (shader/compile-program gl
                                                             vertex-shader-source
                                                             fragment-shader-source)
                            :texture-buffer-id (buffer/create-gl-buffer gl)
                            :quad-coordinate-buffer-id (buffer/create-gl-buffer gl)
                            :parent-buffer-id (buffer/create-gl-buffer gl)
                            :texture-offset-attribute-buffer (buffer/create-gl-buffer gl)
                            :texture-size-attribute-buffer (buffer/create-gl-buffer gl)}))
    @gpu-state-atom))

(defn update-gpu [window gpu-state state]
  (window/render window gl
                 (buffer/load-buffer gl
                                     (:texture-offset-attribute-buffer gpu-state)
                                     :int
                                     (:texture-offsets state))

                 (buffer/load-buffer gl
                                     (:texture-size-attribute-buffer gpu-state)
                                     :short
                                     (:texture-sizes state))

                 (buffer/load-texture-buffer gl
                                             (:texture-buffer-id gpu-state)
                                             :int
                                             (mapcat #(-> % (.getRaster) (.getDataBuffer) (.getData))
                                                     (:images state)))

                 (buffer/load-texture-buffer gl
                                             (:quad-coordinate-buffer-id gpu-state)
                                             :float
                                             (:quad-coordinates state))

                 (buffer/load-texture-buffer gl
                                             (:parent-buffer-id gpu-state)
                                             :float
                                             (:parents state))

                 (.glBindBuffer gl GL2/GL_TEXTURE_BUFFER 0)))

(defn draw [window width height gpu-state number-of-quads]
  (window/render window gl
                 (shader/enable-program gl
                                        (:program gpu-state))

                 (create-vertex-attribute-array gl
                                                "texture_offset_attribute"
                                                (:program gpu-state)
                                                (:texture-offset-attribute-buffer gpu-state)
                                                :int
                                                1
                                                1)

                 (create-vertex-attribute-array gl
                                                "texture_size_attribute"
                                                (:program gpu-state)
                                                (:texture-size-attribute-buffer gpu-state)
                                                :short
                                                2
                                                1)

                 (bind-texture-buffer gl
                                      (:texture-buffer-id gpu-state)
                                      0
                                      (:program gpu-state)
                                      "texture"
                                      GL2/GL_RGBA8)

                 (bind-texture-buffer gl
                                      (:quad-coordinate-buffer-id gpu-state)
                                      1
                                      (:program gpu-state)
                                      "quad_coordinates_buffer"
                                      GL2/GL_RG32UI)

                 (bind-texture-buffer gl
                                      (:parent-buffer-id gpu-state)
                                      2
                                      (:program gpu-state)
                                      "parents"
                                      GL2/GL_R32UI)


                 (shader/set-float4-matrix-uniform gl
                                                   (:program gpu-state)
                                                   "projection_matrix"
                                                   (math/projection-matrix-2d width
                                                                              height
                                                                              1.0))

                 (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 number-of-quads)))


(defn create-state [images coordinates parents]
  {:texture-offsets (loop [offsets [0]
                           images images]
                      (if-let [image (first images)]
                        (recur (conj offsets
                                     (+ (last offsets)
                                        (* (.getWidth image)
                                           (.getHeight image))))
                               (rest images))
                        offsets))

   :texture-sizes (mapcat #(vector (.getWidth %) (.getHeight %))
                          images)

   :quad-coordinates coordinates
   :parents parents
   :images images})


(defn change-texture [gl state gpu-state index new-image]
  (buffer/update-texture-buffer gl
                                (:texture-buffer-id gpu-state)
                                :int
                                (get (:texture-offsets state)
                                     0)
                                (-> new-image (.getRaster) (.getDataBuffer) (.getData)))

  (buffer/update-texture-buffer gl
                                (:texture-size-attribute-buffer gpu-state)
                                :short
                                0
                                [(.getWidth new-image)
                                 (.getHeight new-image)]))

(defn move-quad [gl gpu-state index x y]
  (buffer/update-texture-buffer gl
                                (:quad-coordinate-buffer-id gpu-state)
                                :float
                                (* 2 index)
                                [x y]))

(defn start []
  (let [width 400
        height 400
        window (window/create width height :profile :gl3)
        images (map text-image ["for" "bar" "baz"])]

    (try
      (let [gpu-state (initialize-gpu window)
            state (create-state images
                                [100 10
                                 20 40
                                 20 30]
                                [-1 0 1])]

        (update-gpu window
                    gpu-state
                    state)

        (window/render window gl

                       (change-texture gl
                                       state
                                       gpu-state
                                       0
                                       (text-image "for"))

                       (move-quad gl
                                  gpu-state
                                  1
                                  100
                                  200))

        (draw window
              width
              height
              gpu-state
              (count images)))

      (catch Exception e
        (window/close window)
        (throw e)))))

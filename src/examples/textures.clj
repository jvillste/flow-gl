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
uniform usamplerBuffer texture_offset_attribute;

out vec2 texture_coordinate;
flat out uint texture_offset;
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

    texture_offset = texelFetch(texture_offset_attribute, gl_InstanceID).x;
    texture_width = texture_size_attribute.x;
}
")

(def fragment-shader-source "
#version 140

uniform samplerBuffer texture;

in vec2 texture_coordinate;
flat in uint texture_offset;
flat in float texture_width;

out vec4 outColor;

void main() {
    vec4 color = texelFetch(texture, int(texture_offset + texture_coordinate.y * texture_width + texture_coordinate.x - texture_width / 2));
    outColor = vec4(color.b, color.g, color.r, color.a);
}
")

(defn create-vertex-attribute-array [gl name program buffer-id type size divisor]
  (let [attribute-location (.glGetAttribLocation gl program name)]
    (.glBindBuffer gl GL2/GL_ARRAY_BUFFER buffer-id)
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

(defn text-image [text]
  (text/create-buffered-image [1 1 1 1]
                              (font/create "LiberationSans-Regular.ttf" 40)
                              text))

(defn bind-texture-buffer [gl buffer-id texture-unit program uniform-name type]
  (shader/set-int-uniform gl
                          program
                          uniform-name
                          texture-unit)
  (.glActiveTexture gl (+ texture-unit GL2/GL_TEXTURE0))
  (.glBindTexture gl GL2/GL_TEXTURE_BUFFER (texture/create-gl-texture gl))
  (.glTexBuffer gl GL2/GL_TEXTURE_BUFFER type buffer-id))

(defn allocate-quads [gl number-of-quads]
  (let [quad-coordinate-buffer-id (buffer/create-gl-buffer gl)
        parent-buffer-id (buffer/create-gl-buffer gl)
        texture-offset-attribute-buffer (buffer/create-gl-buffer gl)
        texture-size-attribute-buffer (buffer/create-gl-buffer gl)]


    (buffer/allocate-buffer gl
                            quad-coordinate-buffer-id
                            :float
                            GL2/GL_TEXTURE_BUFFER
                            GL2/GL_STATIC_DRAW
                            (* 2 number-of-quads))

    (buffer/allocate-buffer gl
                            parent-buffer-id
                            :float
                            GL2/GL_TEXTURE_BUFFER
                            GL2/GL_STATIC_DRAW
                            number-of-quads)

    (buffer/allocate-buffer gl
                            texture-offset-attribute-buffer
                            :int
                            GL2/GL_ARRAY_BUFFER
                            GL2/GL_STATIC_DRAW
                            number-of-quads)

    (buffer/allocate-buffer gl
                            texture-size-attribute-buffer
                            :short
                            GL2/GL_ARRAY_BUFFER
                            GL2/GL_STATIC_DRAW
                            (* 2 number-of-quads))

    {:allocated-quads number-of-quads
     :quad-coordinate-buffer-id quad-coordinate-buffer-id
     :parent-buffer-id parent-buffer-id
     :texture-offset-attribute-buffer texture-offset-attribute-buffer
     :texture-size-attribute-buffer texture-size-attribute-buffer}))

(defn allocate-texture [gl number-of-texels]
  (let [texture-buffer-id (buffer/create-gl-buffer gl)]
    (buffer/allocate-buffer gl
                            texture-buffer-id
                            :int
                            GL2/GL_TEXTURE_BUFFER
                            GL2/GL_STATIC_DRAW
                            number-of-texels)

    {:texture-buffer-id texture-buffer-id
     :allocated-texels number-of-texels}))

(defn initialize-gpu [gl]
  (opengl/initialize gl)

  (let [initial-number-of-texels 1024
        initial-number-of-quads 50]

    (conj {:program (shader/compile-program gl
                                            vertex-shader-source
                                            fragment-shader-source)

           :allocated-texels initial-number-of-texels
           :next-free-texel 0
           :allocated-quads initial-number-of-quads
           :next-free-quad 0}

          (allocate-texture gl initial-number-of-texels)
          (allocate-quads gl initial-number-of-quads))))

(defn grow-texture-buffer [gl gpu-state minimum-size]
  (let [new-gpu-state (conj gpu-state
                            (allocate-texture gl
                                              (* 2 minimum-size #_(:allocated-texels gpu-state))))]
    (buffer/copy gl
                 (:texture-buffer-id gpu-state)
                 (:texture-buffer-id new-gpu-state)
                 (* 4 (:allocated-texels gpu-state)))

    (buffer/delete gl (:texture-buffer-id gpu-state))

    new-gpu-state))

(defn copy-quad-buffer [buffer-key size-multiplier {:keys [gl size-key old-gpu-state new-gpu-state]}]
  (buffer/copy gl
               (buffer-key old-gpu-state)
               (buffer-key new-gpu-state)
               (* size-multiplier (:allocated-quads old-gpu-state)))
  (buffer/delete gl (buffer-key old-gpu-state)))

(defn grow-quad-buffers [gl gpu-state]
  (let [new-gpu-state (conj gpu-state
                            (allocate-quads gl
                                            (* 2 (:allocated-quads gpu-state))))

        copy-buffer-arguments {:gl gl
                               :old-gpu-state gpu-state
                               :new-gpu-state new-gpu-state}]

    (copy-quad-buffer :quad-coordinate-buffer-id
                      (* 4 2)
                      copy-buffer-arguments)

    (copy-quad-buffer :parent-buffer-id
                      4
                      copy-buffer-arguments)

    (copy-quad-buffer :texture-offset-attribute-buffer
                      4
                      copy-buffer-arguments)

    (copy-quad-buffer :texture-size-attribute-buffer
                      2
                      copy-buffer-arguments)

    new-gpu-state))

(defn add-quad [gpu-state gl buffered-image parent x y]
  (let [new-gpu-state (if (> (* (.getWidth buffered-image)
                                (.getHeight buffered-image))
                             (- (:allocated-texels gpu-state)
                                (:next-free-texel gpu-state)))
                        (grow-texture-buffer gl gpu-state (+ (* (.getWidth buffered-image)
                                                                (.getHeight buffered-image))
                                                             (:next-free-texel gpu-state)))
                        gpu-state)]
    (let [new-gpu-state (if (= (:allocated-quads new-gpu-state)
                               (:next-free-quad new-gpu-state))
                          (grow-quad-buffers gl new-gpu-state)
                          new-gpu-state)]
      (buffer/update gl
                     (:texture-buffer-id new-gpu-state)
                     :int
                     (:next-free-texel new-gpu-state)
                     (-> buffered-image (.getRaster) (.getDataBuffer) (.getData)))

      (buffer/update gl
                     (:parent-buffer-id new-gpu-state)
                     :float
                     (:next-free-quad new-gpu-state)
                     [parent])

      (buffer/update gl
                     (:quad-coordinate-buffer-id new-gpu-state)
                     :float
                     (* 2 (:next-free-quad new-gpu-state))
                     [x y])

      (buffer/update gl
                     (:texture-size-attribute-buffer new-gpu-state)
                     :short
                     (* 2 (:next-free-quad new-gpu-state))
                     [(.getWidth buffered-image)
                      (.getHeight buffered-image)])

      (buffer/update gl
                     (:texture-offset-attribute-buffer new-gpu-state)
                     :int
                     (:next-free-quad new-gpu-state)
                     [(:next-free-texel new-gpu-state)])

      (assoc new-gpu-state
        :next-free-texel (+ (:next-free-texel gpu-state)
                            (* (.getWidth buffered-image)
                               (.getHeight buffered-image)))
        :next-free-quad (+ 1 (:next-free-quad gpu-state))))))


(defn draw [gpu-state gl width height]
  (shader/enable-program gl
                         (:program gpu-state))

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

  (bind-texture-buffer gl
                       (:texture-offset-attribute-buffer gpu-state)
                       3
                       (:program gpu-state)
                       "texture_offset_attribute"
                       GL2/GL_R32UI)

  (shader/set-float4-matrix-uniform gl
                                    (:program gpu-state)
                                    "projection_matrix"
                                    (math/projection-matrix-2d width
                                                               height
                                                               1.0))

  (shader/validate-program gl (:program gpu-state))
  (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 (:next-free-quad gpu-state)))


(defn change-texture [gpu-state gl index new-image]
  (buffer/update gl
                 (:texture-buffer-id gpu-state)
                 :int
                 (first (buffer/read gl (:texture-offset-attribute-buffer gpu-state) :int index 1))
                 (-> new-image (.getRaster) (.getDataBuffer) (.getData)))

  (buffer/update gl
                 (:texture-size-attribute-buffer gpu-state)
                 :short
                 (* 2 index)
                 [(.getWidth new-image)
                  (.getHeight new-image)])
  gpu-state)

(defn move-quad [gpu-state gl index x y]
  (buffer/update gl
                 (:quad-coordinate-buffer-id gpu-state)
                 :float
                 (* 2 index)
                 [x y])
  gpu-state)

(defn start []
  (let [width 400
        height 400
        window (window/create width height :profile :gl3)
        images (map text-image ["for" "bar" "baz"])]

    (try
      (window/with-gl window gl
        (-> (initialize-gpu gl)
            (add-quad gl (text-image "foo") -1 100 10)
            (add-quad gl (text-image "baaar") -1 100 30)
            #_(move-quad gl 0 10 10)
            #_(change-texture gl 1 (text-image "baaz"))
            (as-> gpu-state
                  (do (println gpu-state)
                      gpu-state))
            (draw gl width height)))
      (catch Exception e
        (window/close window)
        (throw e)))))


;; TODO
;; remove quad
;; resize quad larger

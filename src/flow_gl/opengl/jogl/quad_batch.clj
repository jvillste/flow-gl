(ns flow-gl.opengl.jogl.quad-batch
  (:require [flow-gl.gui.event-queue :as event-queue]
            [clojure.core.matrix :as matrix]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer]
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

(def quad-parameters-size 9)
(def parent-offset 0)
(def x-offset 1)
(def y-offset 2)
(def width-offset 3)
(def height-offset 4)
(def texture-offset-offset 5)
(def quad-width-offset 6)
(def quad-height-offset 7)
(def upside-down-offset 8)

(defn read-quad-parameters [quad-batch gl id]
  (let [[parent x y width height texture-offset] (buffer/read gl
                                                              (:quad-parameters-buffer-id quad-batch)
                                                              :int
                                                              (* quad-parameters-size
                                                                 ((:ids-to-indexes quad-batch) id))
                                                              quad-parameters-size)]
    {:parent parent
     :x x
     :y y
     :width width
     :height height
     :texture-offset texture-offset}))

(defn set-quad-parameters [quad-batch gl id quad-parameters]
  (buffer/update-from-native-buffer gl
                                    (:quad-parameters-buffer-id quad-batch)
                                    :int
                                    (* quad-parameters-size
                                       ((:ids-to-indexes quad-batch) id))
                                    [(:parent quad-parameters)
                                     (:x quad-parameters)
                                     (:y quad-parameters)
                                     (:width quad-parameters)
                                     (:height quad-parameters)
                                     (:texture-offset quad-parameters)])
  quad-batch)

(def vertex-shader-source "
  #version 140

  uniform mat4 projection_matrix;

  uniform int use_quad_index_buffer;

  uniform isamplerBuffer quad_parameters;

  uniform isamplerBuffer quad_index_sampler;

  out vec2 texture_coordinate;
  flat out int texture_offset;
  flat out int texture_width;

  const int quad_parameters_size = 9;

  void main() {

  // 0 parent
  // 1 x
  // 2 y
  // 3 texture width
  // 4 texture height
  // 5 texel index
  // 6 quad width
  // 7 quad height
  // 8 upside down

  int quad_index;
  if(use_quad_index_buffer == 1)
  quad_index = texelFetch(quad_index_sampler, gl_InstanceID).x;
  else
  quad_index = gl_InstanceID;

  ivec2 texture_size = ivec2(texelFetch(quad_parameters, quad_index * quad_parameters_size + 3).x,
  texelFetch(quad_parameters, quad_index * quad_parameters_size + 4).x);

  ivec2 quad_size = ivec2(texelFetch(quad_parameters, quad_index * quad_parameters_size + 6).x,
  texelFetch(quad_parameters, quad_index * quad_parameters_size + 7).x);

  vec2 vertex_coordinates;

  switch(gl_VertexID) {
  case 0:
  texture_coordinate = vec2(0.0, 0.0);
  vertex_coordinates = vec2(0.0, 0.0);
  break;
  case 1:
  texture_coordinate = vec2(0.0, texture_size.y);
  vertex_coordinates = vec2(0.0, quad_size.y);
  break;
  case 2:
  texture_coordinate = vec2(texture_size.x, 0.0);
  vertex_coordinates = vec2(quad_size.x, 0.0);
  break;
  case 3:
  texture_coordinate = vec2(texture_size.x, texture_size.y);
  vertex_coordinates = vec2(quad_size.x, quad_size.y);
  break;
  }

  if(texelFetch(quad_parameters, quad_index * quad_parameters_size + 8).x == 1)
  texture_coordinate.y = texture_size.y - texture_coordinate.y;

  vec2 quad_coordinates = vec2(texelFetch(quad_parameters, quad_index * quad_parameters_size + 1).x,
  texelFetch(quad_parameters, quad_index * quad_parameters_size + 2).x);

  gl_Position = projection_matrix * vec4(vertex_coordinates.x + quad_coordinates.x,
  vertex_coordinates.y + quad_coordinates.y, 0.0, 1.0);

  texture_offset = texelFetch(quad_parameters, quad_index * quad_parameters_size + 5).x;
  texture_width = texture_size.x;
  }
  ")

(def fragment-shader-source "
  #version 140

  uniform samplerBuffer texture;

  in vec2 texture_coordinate;
  flat in int texture_offset;
  flat in int texture_width;

  out vec4 outColor;

  void main() {
  ivec2 texture_int_coordinate;
  texture_int_coordinate = ivec2(int(texture_coordinate.x), int(texture_coordinate.y));

  //vec4 color = texelFetch(texture, int(texture_offset + texture_coordinate.y * texture_width + texture_coordinate.x - int(texture_width) / 2));
  //vec4 color = texelFetch(texture, int(texture_offset + texture_coordinate.y * texture_width + texture_coordinate.x));

  vec4 color;
  color  = texelFetch(texture, int(texture_offset + texture_int_coordinate.y * texture_width + texture_int_coordinate.x));
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

(defn ids [quad-batch]
  (or (keys (:ids-to-indexes quad-batch))
      []))

(defn bind-texture-buffer [gl buffer-id texture-id texture-unit program uniform-name type]
  (shader/set-int-uniform gl
                          program
                          uniform-name
                          texture-unit)
  (.glActiveTexture gl (+ texture-unit GL2/GL_TEXTURE0))
  (.glBindTexture gl GL2/GL_TEXTURE_BUFFER texture-id)
  (.glTexBuffer gl GL2/GL_TEXTURE_BUFFER type buffer-id))

(defn allocate-quads [gl number-of-quads]
  (let [quad-parameters-buffer (buffer/create-gl-buffer gl)]

    (buffer/allocate-buffer gl
                            quad-parameters-buffer
                            :int
                            GL2/GL_ARRAY_BUFFER
                            GL2/GL_DYNAMIC_DRAW
                            (* quad-parameters-size number-of-quads))
    quad-parameters-buffer))

(defn allocate-texture [gl number-of-texels]
  (let [texture-buffer-id (buffer/create-gl-buffer gl)]
    (buffer/allocate-buffer gl
                            texture-buffer-id
                            :int
                            GL2/GL_TEXTURE_BUFFER
                            GL2/GL_STATIC_DRAW
                            number-of-texels)
    texture-buffer-id))

(defn create [gl]
  ;;(opengl/initialize gl)

  (let [initial-number-of-texels 20000
        initial-number-of-quads 100
        quad-batch {:program (shader/compile-program gl
                                                     vertex-shader-source
                                                     fragment-shader-source)

                    :int-buffer (native-buffer/create-native-buffer :int (* initial-number-of-quads quad-parameters-size))
                    :next-free-texel 0
                    :next-free-quad 0
                    :removed-quads 0
                    :removed-texels 0
                    :ids-to-indexes {}
                    :next-free-id 0

                    :textures-in-use {}
                    :next-free-texture-id 0

                    :vertex-array-object (vertex-array-object/create gl)

                    :allocated-quads initial-number-of-quads
                    :quad-parameters-buffer-id (allocate-quads gl initial-number-of-quads)

                    :texture-buffer-id (allocate-texture gl initial-number-of-texels)
                    :texture-buffer-texture-id (texture/create-texture-object gl)
                    :quad-parameters-buffer-texture-id (texture/create-texture-object gl)

                    :allocated-texels initial-number-of-texels}]

    (vertex-array-object/bind gl (:vertex-array-object quad-batch))
    (shader/validate-program gl (:program quad-batch))
    quad-batch))



(defn collect-garbage [quad-batch gl]
  (if (= (:next-free-quad quad-batch)
         0)
    quad-batch
    (let [new-number-of-quads (- (:next-free-quad quad-batch)
                                 (:removed-quads quad-batch))

          new-quad-parameters-buffer-size (max 100
                                               (* new-number-of-quads
                                                  2))

          new-number-of-texels (- (:next-free-texel quad-batch)
                                  (:removed-texels quad-batch))

          new-texture-buffer-size (max 10000
                                       (* new-number-of-texels
                                          2))

          quad-parameters-native-buffer (native-buffer/ensure-buffer-capacity (:int-buffer quad-batch)
                                                                              (* quad-parameters-size
                                                                                 new-quad-parameters-buffer-size))

          new-quad-parameters-buffer (buffer/create-gl-buffer gl)

          new-texture-buffer (allocate-texture gl
                                               new-texture-buffer-size)

          quad-parameters (buffer/read gl
                                       (:quad-parameters-buffer-id quad-batch)
                                       :int
                                       0
                                       (* quad-parameters-size
                                          (:next-free-quad quad-batch)))

          ids-to-indexes (loop [remaining-ids (ids quad-batch)
                                texture-offset 0
                                new-index 0
                                ids-to-indexes {}]

                           (if (seq remaining-ids)
                             (let [id (first remaining-ids)
                                   old-index (get (:ids-to-indexes quad-batch) id)
                                   current-quad-parameters (vec (java.util.Arrays/copyOfRange quad-parameters
                                                                                              (* quad-parameters-size
                                                                                                 old-index)
                                                                                              (+ (* quad-parameters-size
                                                                                                    old-index)
                                                                                                 quad-parameters-size)))
                                   width (get current-quad-parameters width-offset)
                                   height (get current-quad-parameters height-offset)]

                               (.put quad-parameters-native-buffer (int-array (assoc current-quad-parameters
                                                                                     texture-offset-offset
                                                                                     texture-offset)))

                               (buffer/copy gl
                                            (:texture-buffer-id quad-batch)
                                            new-texture-buffer
                                            :int
                                            (get current-quad-parameters texture-offset-offset)
                                            texture-offset
                                            (* width height))

                               (recur (rest remaining-ids)
                                      (+ texture-offset
                                         (* width height))
                                      (inc new-index)
                                      (assoc ids-to-indexes id new-index)))

                             ids-to-indexes))]
      (.rewind quad-parameters-native-buffer)
      (buffer/load-buffer gl
                          new-quad-parameters-buffer
                          :int
                          GL2/GL_ARRAY_BUFFER
                          GL2/GL_STATIC_DRAW
                          quad-parameters-native-buffer)
      (buffer/delete gl (:texture-buffer-id quad-batch))
      (buffer/delete gl (:quad-parameters-buffer-id quad-batch))

      (-> quad-batch
          (assoc :int-buffer quad-parameters-native-buffer
                 :texture-buffer-id new-texture-buffer
                 :quad-parameters-buffer-id new-quad-parameters-buffer
                 :allocated-quads new-quad-parameters-buffer-size
                 :next-free-quad new-number-of-quads
                 :allocated-texels new-texture-buffer-size
                 :next-free-texel new-number-of-texels
                 :ids-to-indexes ids-to-indexes
                 :removed-quads 0
                 :removed-texels 0)))))

(defn collect-texture-garbage [quad-batch gl]
  (let [new-number-of-texels (- (:next-free-texel quad-batch)
                                (:removed-texels quad-batch))

        new-texture-buffer-size (max 100000
                                     (* new-number-of-texels
                                        2))

        new-texture-buffer (allocate-texture gl
                                             new-texture-buffer-size)

        textures-in-use (loop [remaining-ids (keys (:textures-in-use quad-batch))
                               texture-offset 0
                               textures-in-use (:textures-in-use quad-batch)]

                          (if-let [id (first remaining-ids)]
                            (let [{:keys [width height first-texel]} (get-in quad-batch [:textures-in-use id])]
                              (do (buffer/copy gl
                                               (:texture-buffer-id quad-batch)
                                               new-texture-buffer
                                               :int
                                               first-texel
                                               texture-offset
                                               (* width height))

                                  (recur (rest remaining-ids)
                                         (+ texture-offset
                                            (* width height))
                                         (assoc-in textures-in-use [id :first-texel] texture-offset))))

                            textures-in-use))]
    (buffer/delete gl (:texture-buffer-id quad-batch))

    (-> quad-batch
        (assoc :texture-buffer-id new-texture-buffer
               :allocated-texels new-texture-buffer-size
               :next-free-texel new-number-of-texels
               :textures-in-use textures-in-use
               :removed-texels 0))))

(defn grow-texture-buffer [gl quad-batch minimum-size]
  (let [new-quad-batch (assoc quad-batch
                              :texture-buffer-id (allocate-texture gl (* 2 minimum-size))
                              :allocated-texels (* 2 minimum-size))]
    (buffer/copy gl
                 (:texture-buffer-id quad-batch)
                 (:texture-buffer-id new-quad-batch)
                 :int
                 0
                 0
                 (:allocated-texels quad-batch))

    (buffer/delete gl (:texture-buffer-id quad-batch))

    new-quad-batch))

(defn grow-quad-buffers [gl quad-batch minimum-size]
  (let [new-quad-batch (assoc quad-batch
                              :allocated-quads (* 2 minimum-size)
                              :quad-parameters-buffer-id (allocate-quads gl (* 2 minimum-size)))]

    (buffer/copy gl
                 (:quad-parameters-buffer-id quad-batch)
                 (:quad-parameters-buffer-id new-quad-batch)
                 :int
                 0
                 0
                 (* quad-parameters-size
                    (:allocated-quads quad-batch)))

    (buffer/delete gl (:quad-parameters-buffer-id quad-batch))

    new-quad-batch))

(defn prepare-for-adding-textures [quad-batch gl texel-count]
  (let [quad-batch (if (> (:removed-texels quad-batch)
                          (/ (:allocated-texels quad-batch)
                             2))
                     (collect-texture-garbage quad-batch gl)
                     quad-batch)

        minimum-texel-capacity (+ texel-count
                                  (:next-free-texel quad-batch))]

    (if (< (:allocated-texels quad-batch)
           minimum-texel-capacity)
      (grow-texture-buffer gl
                           quad-batch
                           minimum-texel-capacity)
      quad-batch)))

(defn finish-adding-textures [quad-batch texel-count textures]
  (assoc quad-batch
         :next-free-texel (+ (:next-free-texel quad-batch)
                             texel-count)
         :textures-in-use (loop [textures-in-use (:textures-in-use quad-batch)
                                 next-free-texel (:next-free-texel quad-batch)
                                 next-free-texture-id (:next-free-texture-id quad-batch)
                                 textures textures]
                            (if-let [texture (first textures)]
                              (recur (assoc textures-in-use
                                            next-free-texture-id {:first-texel next-free-texel
                                                                  :width (:width texture)
                                                                  :height (:height texture)
                                                                  :upside-down (:upside-down texture)})
                                     (+ next-free-texel (* (:width texture)
                                                           (:height texture)))
                                     (inc next-free-texture-id)
                                     (rest textures))
                              textures-in-use))
         :next-free-texture-id (+ (:next-free-texture-id quad-batch)
                                  (count textures))))

(defn texel-count [dimensions]
  (reduce (fn [texel-count dimension]
            (+ texel-count
               (* (:width dimension)
                  (:height dimension))))
          0
          dimensions))

(defn add-textures [quad-batch gl images]
  (let [dimensions (map (fn [image]
                          {:width (.getWidth image)
                           :height (.getHeight image)})
                        images)

        texel-count (texel-count dimensions)

        quad-batch (prepare-for-adding-textures quad-batch gl texel-count)

        buffer (buffer/map-for-write gl
                                     (:texture-buffer-id quad-batch)
                                     :int
                                     (:next-free-texel quad-batch)
                                     texel-count)]

    (doseq [image images]
      (.put buffer
            (-> image
                (.getRaster)
                (.getDataBuffer)
                (.getData))))

    (buffer/unmap-for-write gl)

    (finish-adding-textures quad-batch texel-count dimensions)))


(defn add-textures-from-gl-textures [quad-batch gl textures]
  (let [texel-count (texel-count textures)

        quad-batch (prepare-for-adding-textures quad-batch gl texel-count)]

    (loop [textures textures
           offset (:next-free-texel quad-batch)]
      (if-let [texture (first textures)]
        (do (texture/copy-to-buffer gl
                                    (:texture-id texture)
                                    (:texture-buffer-id quad-batch)
                                    (* 4 offset))
            (recur (rest textures)
                   (+ offset (* (:width texture)
                                (:height texture)))))))

    (finish-adding-textures quad-batch texel-count (map #(assoc % :upside-down true)
                                                        textures))))

(defn draw [quad-batch gl width height model-matrix]

  (vertex-array-object/bind gl (:vertex-array-object quad-batch))

  (shader/enable-program gl
                         (:program quad-batch))

  (bind-texture-buffer gl
                       (:texture-buffer-id quad-batch)
                       (:texture-buffer-texture-id quad-batch)
                       0
                       (:program quad-batch)
                       "texture"
                       GL2/GL_RGBA8)

  (bind-texture-buffer gl
                       (:quad-parameters-buffer-id quad-batch)
                       (:quad-parameters-buffer-texture-id quad-batch)
                       1
                       (:program quad-batch)
                       "quad_parameters"
                       GL2/GL_R32UI)

  (shader/set-float4-matrix-uniform gl
                                    (:program quad-batch)
                                    "projection_matrix"

                                    (math/core-matrix-to-opengl-matrix (if model-matrix
                                                                         (matrix/mmul (math/projection-matrix-2d width
                                                                                                                 height)
                                                                                      model-matrix)

                                                                         (math/projection-matrix-2d width
                                                                                                    height))))

  (shader/set-int-uniform gl
                          (:program quad-batch)
                          "use_quad_index_buffer"
                          0)


  (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 (:next-free-quad quad-batch))

  (vertex-array-object/bind gl 0)

  quad-batch)

(defn draw-quads
  ([quad-batch gl quads width height]
   (draw-quads quad-batch gl quads width height nil))

  ([quad-batch gl quads width height model-matrix]
   #_(flow-gl.debug/set-metric :draw-quads (System/currentTimeMillis))
   #_(flow-gl.debug/set-metric :allocated-quads (:allocated-quads quad-batch))
   #_(flow-gl.debug/set-metric :quad-count (count quads))
   (let [quad-count (count quads)

         quad-batch (if (< (:allocated-quads quad-batch)
                           quad-count)
                      (grow-quad-buffers gl
                                         quad-batch
                                         quad-count)
                      quad-batch)]

     (let [count (* quad-parameters-size
                    quad-count)
           ^IntBuffer buffer (native-buffer/ensure-buffer-capacity (:int-buffer quad-batch)
                                                                   count)]


       (doseq [quad quads]
         (let [texture (if (contains? quad :texture-id)
                         (get (:textures-in-use quad-batch)
                              (:texture-id quad))
                         {:first-texel 0
                          :width 0
                          :height 0
                          :upside-down false})]
           (.put buffer (int (or (:parent quad) -1)))
           (.put buffer (int (:x quad)))
           (.put buffer (int (:y quad)))
           (.put buffer (int (:width texture)))
           (.put buffer (int (:height texture)))
           (.put buffer (int (:first-texel texture)))
           (.put buffer (int (or (:width quad)
                                 ^int      (:width texture))))
           (.put buffer (int (or (:height quad)
                                 ^int      (:height texture))))
           (.put buffer (int (if (:upside-down texture)
                               1
                               0)))))

       (.rewind buffer)

       (buffer/update-from-native-buffer gl
                                         (:quad-parameters-buffer-id quad-batch)
                                         :int
                                         0
                                         count
                                         buffer)

       (draw (assoc quad-batch
                    :draw-count (inc (or (:draw-count quad-batch)
                                         0))
                    :int-buffer buffer
                    :next-free-quad quad-count) gl width height model-matrix)))))



#_(defn remove-index [quad-batch gl index]
    (let [[width height] (buffer/read gl
                                      (:quad-parameters-buffer-id quad-batch)
                                      :int
                                      (+ width-offset (* quad-parameters-size index))
                                      2)]
      (buffer/update gl
                     (:quad-parameters-buffer-id quad-batch)
                     :int
                     (+ width-offset (* quad-parameters-size index))
                     [0 0])

      (-> quad-batch
          (update-in [:removed-quads] inc)
          (update-in [:removed-texels] + (* width height)))))


(defn remove-texture [quad-batch texture-id]

  
  (let [texture (get (:textures-in-use quad-batch)
                     texture-id)]

    (-> quad-batch
        (update-in [:removed-texels] + (* (:width texture)
                                          (:height texture)))
        (update-in [:textures-in-use] dissoc texture-id))))

#_(defn remove-quad [quad-batch gl id]
    (-> quad-batch
        (remove-index gl (get (:ids-to-indexes quad-batch)
                              id))
        (update-in [:ids-to-indexes] dissoc id)))

#_(defn change-texture [quad-batch gl id new-image]
    (let [quad-parameters (read-quad-parameters quad-batch gl id)

          this-is-the-last-texture (= (:texture-offset quad-parameters)
                                      (- (:next-free-texel quad-batch)
                                         (* (:width quad-parameters)
                                            (:height quad-parameters))))]
      (if (and (<= (* (.getWidth new-image)
                      (.getHeight new-image))
                   (* (:width quad-parameters)
                      (:height quad-parameters))))

        (do (buffer/update gl
                           (:texture-buffer-id quad-batch)
                           :int
                           (:texture-offset quad-parameters)
                           (-> new-image (.getRaster) (.getDataBuffer) (.getData)))

            (set-quad-parameters quad-batch gl id
                                 (assoc quad-parameters
                                        :width (.getWidth new-image)
                                        :height (.getHeight new-image)))

            (if this-is-the-last-texture
              (assoc quad-batch
                     :next-free-texel (+ (:texture-offset quad-parameters)
                                         (* (.getWidth new-image)
                                            (.getHeight new-image))))
              quad-batch))

        (do (let [new-texture-offset (if this-is-the-last-texture
                                       (:texture-offset quad-parameters)
                                       (:next-free-texel quad-batch))

                  new-quad-batch (add-textures (if this-is-the-last-texture
                                                 (assoc quad-batch :next-free-texel (:texture-offset quad-parameters))
                                                 quad-batch)
                                               gl
                                               [new-image])]

              (set-quad-parameters new-quad-batch gl id
                                   (assoc quad-parameters
                                          :texture-offset new-texture-offset
                                          :width (.getWidth new-image)
                                          :height (.getHeight new-image)))
              new-quad-batch)))))

#_(defn move-quad [quad-batch gl index x y]
    (buffer/update gl
                   (:quad-parameters-buffer-id quad-batch)
                   :int
                   (+ x-offset (* quad-parameters-size index))
                   [x y])
    quad-batch)


;; TODO
;; optimize updating the same quads constantly. generational GC?
;; share texture
;; group quads to tiles and draw given tiles only
;; load new quads asynchronously in small batches. Ready made byte buffers to be loaded to the GPU

#_(flow-gl.profiling/profile-ns 'flow-gl.opengl.jogl.quad-batch)

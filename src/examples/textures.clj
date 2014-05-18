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

(def quad-parameters-size (+ 2 1 1 2))
(def parent-offset 0)
(def x-offset 1)
(def y-offset 2)
(def width-offset 3)
(def height-offset 4)
(def texture-offset-offset 5)

(defn read-quad-parameters [gpu-state gl id]
  (let [[parent x y width height texture-offset] (buffer/read gl
                                                              (:quad-parameters-buffer-id gpu-state)
                                                              :int
                                                              (* quad-parameters-size
                                                                 ((:ids-to-indexes gpu-state) id))
                                                              quad-parameters-size)]
    {:parent parent
     :x x
     :y y
     :width width
     :height height
     :texture-offset texture-offset}))

(defn set-quad-parameters [gpu-state gl id quad-parameters]
  (buffer/update gl
                 (:quad-parameters-buffer-id gpu-state)
                 :int
                 (* quad-parameters-size
                    ((:ids-to-indexes gpu-state) id))
                 [(:parent quad-parameters)
                  (:x quad-parameters)
                  (:y quad-parameters)
                  (:width quad-parameters)
                  (:height quad-parameters)
                  (:texture-offset quad-parameters)])
  gpu-state)

(def vertex-shader-source "
#version 140

uniform mat4 projection_matrix;

uniform int use_quad_index_buffer;

uniform usamplerBuffer quad_parameters;

uniform isamplerBuffer quad_index_sampler;

out vec2 texture_coordinate;
flat out uint texture_offset;
flat out uint texture_width;

const int quad_parameters_size = 6;

void main() {

    // 0 parent
    // 1 x
    // 2 y
    // 3 (.getWidth buffered-image)
    // 4 (.getHeight buffered-image)
    // 5 (:next-free-texel new-gpu-state)

    int quad_index;
    if(use_quad_index_buffer == 1)
      quad_index = texelFetch(quad_index_sampler, gl_InstanceID).x;
    else
      quad_index = gl_InstanceID;

    uvec2 texture_size = uvec2(texelFetch(quad_parameters, quad_index * quad_parameters_size + 3).x,
                               texelFetch(quad_parameters, quad_index * quad_parameters_size + 4).x);

    switch(gl_VertexID) {
      case 0:
          texture_coordinate = vec2(0.0, 0.0);
        break;
      case 1:
          texture_coordinate = vec2(0.0, texture_size.y);
        break;
      case 2:
          texture_coordinate = vec2(texture_size.x, 0.0);
        break;
      case 3:
          texture_coordinate = vec2(texture_size.x, texture_size.y);
        break;
    }

    int parent = int(texelFetch(quad_parameters, quad_index * quad_parameters_size).x);

    vec2 offset = vec2(0,0);
    while(parent >= 0)
    {
       uvec2 parent_offset = uvec2(texelFetch(quad_parameters, parent * quad_parameters_size + 1).x,
                                   texelFetch(quad_parameters, parent * quad_parameters_size + 2).x);

       offset = vec2(offset.x + parent_offset.x, offset.y + parent_offset.y);
       parent = int(texelFetch(quad_parameters, parent * quad_parameters_size).x);
    }

    uvec2 quad_coordinates = uvec2(texelFetch(quad_parameters, quad_index * quad_parameters_size + 1).x,
                                   texelFetch(quad_parameters, quad_index * quad_parameters_size + 2).x);

    gl_Position = projection_matrix * vec4(offset.x + texture_coordinate.x + quad_coordinates.x, offset.y +  texture_coordinate.y + quad_coordinates.y, 0.0, 1.0);

    texture_offset = texelFetch(quad_parameters, quad_index * quad_parameters_size + 5).x;
    texture_width = texture_size.x;
}
")

(def fragment-shader-source "
#version 140

uniform samplerBuffer texture;

in vec2 texture_coordinate;
flat in uint texture_offset;
flat in uint texture_width;

out vec4 outColor;

void main() {
    vec4 color = texelFetch(texture, int(texture_offset + texture_coordinate.y * texture_width + texture_coordinate.x - int(texture_width) / 2));
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
                              (font/create "LiberationSans-Regular.ttf" 14)
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
  (let [quad-parameters-buffer (buffer/create-gl-buffer gl)]

    (buffer/allocate-buffer gl
                            quad-parameters-buffer
                            :int
                            GL2/GL_ARRAY_BUFFER
                            GL2/GL_STATIC_DRAW
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

(defn initialize-gpu [gl]
  (opengl/initialize gl)

  (let [initial-number-of-texels 10000
        initial-number-of-quads 100]

    {:program (shader/compile-program gl
                                      vertex-shader-source
                                      fragment-shader-source)

     :next-free-texel 0
     :next-free-quad 0
     :removed-quads 0
     :removed-texels 0
     :ids-to-indexes {}
     :next-free-id 0

     :allocated-quads initial-number-of-quads
     :quad-parameters-buffer-id (allocate-quads gl initial-number-of-quads)

     :texture-buffer-id (allocate-texture gl initial-number-of-texels)
     :allocated-texels initial-number-of-texels}))

(defn collect-garbage [gpu-state gl]
  (let [new-number-of-quads (- (:next-free-quad gpu-state)
                               (:removed-quads gpu-state))

        new-quad-parameters-buffer-size (max 100
                                             (* new-number-of-quads
                                                2))

        new-number-of-texels (- (:next-free-texel gpu-state)
                                (:removed-texels gpu-state))

        new-texture-buffer-size (max 10000
                                     (* new-number-of-texels
                                        2))

        quad-parameters-native-buffer (native-buffer/native-buffer :int (* quad-parameters-size
                                                                           new-quad-parameters-buffer-size))

        new-quad-parameters-buffer (buffer/create-gl-buffer gl)

        new-texture-buffer (allocate-texture gl
                                             new-texture-buffer-size)

        quad-parameters (buffer/read gl
                                     (:quad-parameters-buffer-id gpu-state)
                                     :int
                                     0
                                     (* quad-parameters-size (:next-free-quad gpu-state)))

        ids-to-indexes (loop [remaining-ids (keys (:ids-to-indexes gpu-state))
                              texture-offset 0
                              new-index 0
                              ids-to-indexes {}]

                         (if (seq remaining-ids)
                           (let [id (first remaining-ids)
                                 old-index (get (:ids-to-indexes gpu-state) id)
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
                                          (:texture-buffer-id gpu-state)
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
    (buffer/load-buffer-from-native-buffer gl
                                           new-quad-parameters-buffer
                                           :int
                                           GL2/GL_ARRAY_BUFFER
                                           GL2/GL_STATIC_DRAW
                                           quad-parameters-native-buffer)

    (buffer/delete gl (:texture-buffer-id gpu-state))
    (buffer/delete gl (:quad-parameters-buffer-id gpu-state))

    (assoc gpu-state
      :texture-buffer-id new-texture-buffer
      :quad-parameters-buffer-id new-quad-parameters-buffer
      :allocated-quads new-quad-parameters-buffer-size
      :next-free-quad new-number-of-quads
      :allocated-texels new-texture-buffer-size
      :next-free-texel new-number-of-texels
      :ids-to-indexes ids-to-indexes
      :removed-quads 0
      :removed-texels 0)))

(defn grow-texture-buffer [gl gpu-state minimum-size]
  (let [new-gpu-state (assoc gpu-state
                        :texture-buffer-id (allocate-texture gl (* 2 minimum-size))
                        :allocated-texels (* 2 minimum-size))]
    (buffer/copy gl
                 (:texture-buffer-id gpu-state)
                 (:texture-buffer-id new-gpu-state)
                 :int
                 0
                 0
                 (:allocated-texels gpu-state))

    (buffer/delete gl (:texture-buffer-id gpu-state))

    new-gpu-state))

(defn grow-quad-buffers [gl gpu-state minimum-size]
  (let [new-gpu-state (assoc gpu-state
                        :allocated-quads (* 2 minimum-size)
                        :quad-parameters-buffer-id (allocate-quads gl (* 2 minimum-size)))]

    (buffer/copy gl
                 (:quad-parameters-buffer-id gpu-state)
                 (:quad-parameters-buffer-id new-gpu-state)
                 :int
                 0
                 0
                 (* quad-parameters-size (:allocated-quads gpu-state)))

    (buffer/delete gl (:quad-parameters-buffer-id gpu-state))
    new-gpu-state))

(defn add-textures [gpu-state gl images]
  (let [texel-count (reduce (fn [texel-count image]
                              (+ texel-count
                                 (* (.getWidth image)
                                    (.getHeight image))))
                            0
                            images)

        minimum-texel-capacity (+ texel-count
                                  (:next-free-texel gpu-state))

        new-gpu-state (if (< (:allocated-texels gpu-state)
                             minimum-texel-capacity)
                        (grow-texture-buffer gl
                                             gpu-state
                                             minimum-texel-capacity)
                        gpu-state)]

    (let [buffer (buffer/map-for-write gl
                                       (:texture-buffer-id new-gpu-state)
                                       :int
                                       (:next-free-texel new-gpu-state)
                                       texel-count)]

      (doseq [image images]
        (.put buffer
              (-> image
                  (.getRaster)
                  (.getDataBuffer)
                  (.getData))))

      (buffer/unmap-for-write gl))

    (assoc new-gpu-state
      :next-free-texel (+ (:next-free-texel gpu-state)
                          texel-count))))

(defn add-quads [gpu-state gl quads]
  (let [new-gpu-state (add-textures gpu-state gl (map :image quads))

        quad-count (count quads)

        minimum-quad-capacity (+ (:next-free-quad new-gpu-state)
                                 quad-count)

        new-gpu-state (if (< (:allocated-quads new-gpu-state)
                             minimum-quad-capacity)
                        (grow-quad-buffers gl
                                           new-gpu-state
                                           minimum-quad-capacity)
                        new-gpu-state)

        ids-to-indexes (loop [count quad-count
                              id (:next-free-id gpu-state)
                              index (:next-free-quad gpu-state)
                              ids-to-indexes (:ids-to-indexes gpu-state)]
                         (if (> count 0)
                           (recur (dec count)
                                  (inc id)
                                  (inc index)
                                  (assoc ids-to-indexes id index))
                           ids-to-indexes))]

    (let [buffer (buffer/map-for-write gl
                                       (:quad-parameters-buffer-id new-gpu-state)
                                       :int
                                       (* quad-parameters-size
                                          (:next-free-quad new-gpu-state))
                                       (* quad-parameters-size
                                          quad-count))]

      (loop [texture-offset (:next-free-texel gpu-state)
             quads quads]
        (when-let [quad (first quads)]
          (do (.put buffer
                    (int-array [(:parent quad)
                                (:x quad)
                                (:y quad)
                                (.getWidth (:image quad))
                                (.getHeight (:image quad))
                                texture-offset]))
              (recur (+ texture-offset
                        (* (.getWidth (:image quad))
                           (.getHeight (:image quad))))
                     (rest quads)))))
      (buffer/unmap-for-write gl))

    (assoc new-gpu-state
      :next-free-id (+ (:next-free-id gpu-state)
                       quad-count)
      :next-free-quad (+ (:next-free-quad gpu-state)
                         quad-count)
      :ids-to-indexes ids-to-indexes)))

(defn draw [gpu-state gl width height]
  (shader/enable-program gl
                         (:program gpu-state))

  (bind-texture-buffer gl
                       (:texture-buffer-id gpu-state)
                       0
                       (:program gpu-state)
                       "texture"
                       GL2/GL_RGBA8)

  (bind-texture-buffer gl
                       (:quad-parameters-buffer-id gpu-state)
                       1
                       (:program gpu-state)
                       "quad_parameters"
                       GL2/GL_R32UI)

  (shader/set-float4-matrix-uniform gl
                                    (:program gpu-state)
                                    "projection_matrix"
                                    (math/projection-matrix-2d width
                                                               height))
  (shader/set-int-uniform gl
                          (:program gpu-state)
                          "use_quad_index_buffer"
                          0)

  (shader/validate-program gl (:program gpu-state))

  (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 (:next-free-quad gpu-state))

  gpu-state)

(defn draw-indexes [gpu-state gl width height indexes]
  (let [quad-index-buffer (buffer/create-gl-buffer gl)]
    (shader/enable-program gl
                           (:program gpu-state))

    (buffer/load-buffer gl
                        quad-index-buffer
                        :int
                        GL2/GL_ARRAY_BUFFER
                        GL2/GL_STATIC_DRAW
                        indexes)

    (bind-texture-buffer gl
                         quad-index-buffer
                         0
                         (:program gpu-state)
                         "quad_index_sampler"
                         GL2/GL_R32I)

    (bind-texture-buffer gl
                         (:texture-buffer-id gpu-state)
                         1
                         (:program gpu-state)
                         "texture"
                         GL2/GL_RGBA8)

    (bind-texture-buffer gl
                         (:quad-parameters-buffer-id gpu-state)
                         2
                         (:program gpu-state)
                         "quad_parameters"
                         GL2/GL_R32UI)

    (shader/set-float4-matrix-uniform gl
                                      (:program gpu-state)
                                      "projection_matrix"
                                      (math/projection-matrix-2d width
                                                                 height))
    (shader/set-int-uniform gl
                            (:program gpu-state)
                            "use_quad_index_buffer"
                            1)

    (shader/validate-program gl (:program gpu-state))

    (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 (count indexes))

    gpu-state))

(defn remove-quad [gpu-state gl id]
  (let [index (get (:ids-to-indexes gpu-state)
                   id)
        [width height] (buffer/read gl
                                    (:quad-parameters-buffer-id gpu-state)
                                    :int
                                    (+ width-offset (* quad-parameters-size index))
                                    2)]
    (buffer/update gl
                   (:quad-parameters-buffer-id gpu-state)
                   :int
                   (+ width-offset (* quad-parameters-size index))
                   [0 0])

    (-> gpu-state
        (update-in [:removed-quads] inc)
        (update-in [:removed-texels] + (* width height))
        (update-in [:ids-to-indexes] dissoc id))))

(defn change-texture [gpu-state gl id new-image]
  (let [quad-parameters (read-quad-parameters gpu-state gl id)

        this-is-the-last-texture (= (:texture-offset quad-parameters)
                                    (- (:next-free-texel gpu-state)
                                       (* (:width quad-parameters)
                                          (:height quad-parameters))))]
    (if (and (<= (* (.getWidth new-image)
                    (.getHeight new-image))
                 (* (:width quad-parameters)
                    (:height quad-parameters))))

      (do (buffer/update gl
                         (:texture-buffer-id gpu-state)
                         :int
                         (:texture-offset quad-parameters)
                         (-> new-image (.getRaster) (.getDataBuffer) (.getData)))

          (set-quad-parameters gpu-state gl id
                               (assoc quad-parameters
                                 :width (.getWidth new-image)
                                 :height (.getHeight new-image)))

          (if this-is-the-last-texture
            (assoc gpu-state
              :next-free-texel (+ (:texture-offset quad-parameters)
                                  (* (.getWidth new-image)
                                     (.getHeight new-image))))
            gpu-state))

      (do (let [new-texture-offset (if this-is-the-last-texture
                                     (:texture-offset quad-parameters)
                                     (:next-free-texel gpu-state))

                new-gpu-state (add-textures (if this-is-the-last-texture
                                              (assoc gpu-state :next-free-texel (:texture-offset quad-parameters))
                                              gpu-state)
                                            gl
                                            [new-image])]

            (set-quad-parameters new-gpu-state gl id
                                 (assoc quad-parameters
                                   :texture-offset new-texture-offset
                                   :width (.getWidth new-image)
                                   :height (.getHeight new-image)))
            new-gpu-state)))))

(defn move-quad [gpu-state gl index x y]
  (buffer/update gl
                 (:quad-parameters-buffer-id gpu-state)
                 :int
                 (+ x-offset (* quad-parameters-size index))
                 [x y])
  gpu-state)

(defn wait-for-frame-end-time [frame-start-time framerate]
  (let [frame-length (/ 1E9
                        framerate)
        time-spent-until-now (- (System/nanoTime)
                                frame-start-time)]

    (Thread/sleep (max 0
                       (/ (- frame-length
                             time-spent-until-now)
                          1000000)))))

(defn render-loop [gpu-state window]
  (loop [gpu-state gpu-state
         frame-start-time (System/nanoTime)]
    (let [new-gpu-state (window/with-gl window gl
                          (opengl/clear gl 0 0 0 1)
                          (-> gpu-state
                              (move-quad gl
                                         0
                                         (* (/ (mod (System/nanoTime)
                                                    (* 10 1E9))
                                               (* 10 1E9))
                                            400)
                                         0))
                          (draw gpu-state
                                gl
                                (window/width window)
                                (window/height window)))]

      (wait-for-frame-end-time frame-start-time 60)
      (when (.isVisible (:gl-window window))
        (recur new-gpu-state
               (System/nanoTime))))))

(defn add-test-quads-batch [gpu-state gl]
  (-> gpu-state
      (add-quads gl
                 [{:image (text-image "parent")
                   :x 10
                   :y 30
                   :parent -1}])

      (add-quads gl
                   (let [per-column 25
                         column-width 40
                         row-height 16]
                     (for [number (range 300)]
                       {:image (text-image (str number))
                        :x (int (* (Math/floor (/ number per-column)) column-width))
                        :y (int (+ 20 (* (mod number per-column) row-height)))
                        :parent 0})))
      (remove-quad gl 3)
      (as-> gpu-state
            (do (println "before" gpu-state)
                gpu-state))
      (change-texture gl 0 (text-image "par"))
      (collect-garbage gl)
      (as-> gpu-state
            (do (println "after" gpu-state)
                gpu-state))))

(defn start []
  (let [width 500
        height 500
        ;;event-queue (event-queue/create)
        window (window/create width height :profile :gl3 #_:event-queue #_event-queue)
        images (map text-image ["for" "bar" "baz"])]

    (try
      (let [gpu-state (window/with-gl window gl
                        (-> (initialize-gpu gl)
                            (add-test-quads-batch gl)
                            (as-> gpu-state
                                  (do ;; (println gpu-state)
                                    ;;(println "quad index" (seq (buffer/read gl (:quad-index-buffer gpu-state) :int 0 (:next-free-quad gpu-state))))
                                    ;;(println "quad coordinate" (seq (buffer/read gl (:quad-coordinate-buffer-id gpu-state) :float 0 (* 2 (:next-free-quad gpu-state)))))
                                    ;;(println "texture offset" (seq (buffer/read gl (:texture-offset-attribute-buffer gpu-state) :int 0 (:next-free-quad gpu-state))))

                                    ;;(println "texture size" (seq (buffer/read gl (:texture-size-attribute-buffer gpu-state) :short 0 (* 2 (:next-free-quad gpu-state)))))
                                    gpu-state))
                            #_(draw gl width height)))]
        (render-loop gpu-state window))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))

;; TODO
;; optimize updating the same quads constantly. generational GC?
;; share texture
;; group quads to tiles and draw given tiles only
;; load new quads asynchronously in small batches. Ready made byte buffers to be loaded to the GPU

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

(def vertex-shader-source "
#version 140

uniform mat4 projection_matrix;

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

    int quad_index = texelFetch(quad_index_sampler, gl_InstanceID).x;

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
  (let [quad-index-buffer (buffer/create-gl-buffer gl)
        quad-parameters-buffer (buffer/create-gl-buffer gl)]

    (buffer/allocate-buffer gl
                            quad-parameters-buffer
                            :int
                            GL2/GL_ARRAY_BUFFER
                            GL2/GL_STATIC_DRAW
                            (* quad-parameters-size number-of-quads))


    (buffer/allocate-buffer gl
                            quad-index-buffer
                            :int
                            GL2/GL_ARRAY_BUFFER
                            GL2/GL_STATIC_DRAW
                            number-of-quads)

    {:allocated-quads number-of-quads
     :quad-parameters-buffer-id quad-parameters-buffer
     :quad-index-buffer quad-index-buffer}))

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

  (let [initial-number-of-texels 10000
        initial-number-of-quads 100]

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

(defn grow-quad-buffers [gl gpu-state minimum-size]
  (let [new-gpu-state (conj gpu-state
                            (allocate-quads gl
                                            (* 2 minimum-size)))

        copy-buffer-arguments {:gl gl
                               :old-gpu-state gpu-state
                               :new-gpu-state new-gpu-state}]

    (copy-quad-buffer :quad-parameters-buffer-id
                      (* 4 quad-parameters-size)
                      copy-buffer-arguments)


    (copy-quad-buffer :quad-index-buffer
                      4
                      copy-buffer-arguments)

    new-gpu-state))

(defn add-quads [gpu-state gl quads]
  (let [quad-count (count quads)
        texel-count (reduce (fn [texel-count quad]
                              (+ texel-count
                                 (* (.getWidth (:image quad))
                                    (.getHeight (:image quad)))))
                            0
                            quads)

        minimum-texel-capacity (+ texel-count
                                  (:next-free-texel gpu-state))

        new-gpu-state (if (< (:allocated-texels gpu-state)
                             minimum-texel-capacity)
                        (grow-texture-buffer gl
                                             gpu-state
                                             minimum-texel-capacity)
                        gpu-state)

        minimum-quad-capacity (+ (:next-free-quad new-gpu-state)
                                 quad-count)

        new-gpu-state (if (< (:allocated-quads new-gpu-state)
                             minimum-quad-capacity)
                        (grow-quad-buffers gl
                                           new-gpu-state
                                           minimum-quad-capacity)
                        new-gpu-state)]

    (let [buffer (buffer/map-for-write gl
                                       (:texture-buffer-id new-gpu-state)
                                       :int
                                       (:next-free-texel new-gpu-state)
                                       texel-count)]
      (doseq [quad quads]
        (.put buffer
              (-> (:image quad)
                  (.getRaster)
                  (.getDataBuffer)
                  (.getData))))
      (buffer/unmap-for-write gl))

    (let [buffer (buffer/map-for-write gl
                                       (:quad-parameters-buffer-id new-gpu-state)
                                       :int
                                       (* quad-parameters-size
                                          (:next-free-quad new-gpu-state))
                                       (* quad-parameters-size
                                          quad-count))]
      (loop [texture-offset (:next-free-texel new-gpu-state)
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

    (buffer/update gl
                   (:quad-index-buffer new-gpu-state)
                   :int
                   (:next-free-quad new-gpu-state)
                   (range (:next-free-quad new-gpu-state)
                          (+ (:next-free-quad new-gpu-state)
                             quad-count)))

    (assoc new-gpu-state
      :next-free-texel (+ (:next-free-texel gpu-state)
                          texel-count)
      :next-free-quad (+ (:next-free-quad gpu-state)
                         quad-count))))

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
                       (:quad-index-buffer gpu-state)
                       5
                       (:program gpu-state)
                       "quad_index_sampler"
                       GL2/GL_R32I)

  (bind-texture-buffer gl
                       (:quad-parameters-buffer-id gpu-state)
                       6
                       (:program gpu-state)
                       "quad_parameters"
                       GL2/GL_R32UI)

  (shader/set-float4-matrix-uniform gl
                                    (:program gpu-state)
                                    "projection_matrix"
                                    (math/projection-matrix-2d width
                                                               height))

  (shader/validate-program gl (:program gpu-state))

  (.glDrawArraysInstanced gl GL2/GL_TRIANGLE_STRIP 0 4 (:next-free-quad gpu-state))

  gpu-state)

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
                 (:quad-parameters-buffer-id gpu-state)
                 :int
                 (+ x-offset
                    (* quad-parameters-size index))
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
                                                    1E9)
                                               1E9)
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
                      :parent 0})))))

(defn start []
  (let [width 500
        height 500
        ;;event-queue (event-queue/create)
        window (window/create width height :profile :gl3 #_:event-queue #_event-queue)
        images (map text-image ["for" "bar" "baz"])]

    (try
      (let [gpu-state (window/with-gl window gl
                        (-> (initialize-gpu gl)
                            #_(add-quad gl (buffered-image/create-from-file "pumpkin.png") -1 10 10)
                            (add-test-quads-batch gl)
                            #_(add-quad gl (text-image "foo") -1 100 10)
                            #_(add-quad gl (text-image "baaar") -1 100 30)
                            #_(move-quad gl 0 10 10)
                            #_(change-texture gl 1 (text-image "baaz"))
                            (as-> gpu-state
                                  (do (println gpu-state)
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
;; combine buffers
;; load multiple quads with single buffer mapping
;; remove quad
;; resize quad larger
;; share texture
;; collect garbage

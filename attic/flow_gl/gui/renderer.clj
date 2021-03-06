(ns flow-gl.gui.renderer
  (:require
   (flow-gl.gui [drawable :as drawable]
                [quad-view :as quad-view])

   (flow-gl.opengl.jogl [opengl :as opengl]
                        [render-target :as render-target]
                        [quad :as quad]
                        [shader :as shader]
                        [multicolor-triangle-list :as multicolor-triangle-list]))
  (:import [flow_gl.gui.drawable Quad]
           [com.jogamp.opengl GL2]))

(defprotocol Renderer
  (can-draw? [this drawable])

  (draw-drawables [this drawables gl])

  (start-frame [this gl])

  (end-frame [this gl])

  (delete [this gl]))

(defn map-for-renderers [function gl renderers]
  (doall (map #(function % gl)
              renderers)))

(defn select-renderer [renderers drawable]
  (loop [renderers renderers]
    (when-let [renderer (first renderers)]
      (if (can-draw? renderer drawable)
        renderer
        (recur (rest renderers))))))

(defn group-by-renderers [renderers drawables]
  (group-by (partial select-renderer renderers)
            drawables))

(defn render-drawables-with-renderers [drawables gl renderers]
  (let [batches (group-by-renderers renderers drawables)]
    (loop [renderers renderers
           rendered-renderers []]
      (if-let [renderer (first renderers)]
        (recur (rest renderers)
               (conj rendered-renderers
                     (if-let [batch (get batches renderer)]
                       (draw-drawables renderer
                                       batch
                                       gl)
                       renderer)))
        rendered-renderers))))

(defn render-layers [layers gl renderers]
  (loop [renderers renderers
         layers layers]
    (if-let [layer-drawables (first layers)]
      (recur (render-drawables-with-renderers layer-drawables gl renderers)
             (rest layers))
      renderers)))

(defn z-comparator [xs ys]
  (loop [xs xs
         ys ys]
    (let [x (first xs)
          y (first ys)]
      (if (and (and x y)
               (= x y))
        (recur (rest xs) (rest ys))
        (if (= x y nil)
          0
          (compare (or x 0) (or y 0)))))))

(defn create-layers [drawables]
  (->> drawables
       (sort-by :z z-comparator)
       (partition-by :z)))

(defn render-frame-drawables [drawables gl renderers]
  (render-layers (create-layers drawables)
                 gl
                 renderers))

(defn render-frame [drawables gl renderers]
  (->> renderers
       (map-for-renderers start-frame gl)
       (render-frame-drawables drawables gl)
       (map-for-renderers end-frame gl)))


(defrecord TriangleListRenderer [triangle-list]
  Renderer
  (can-draw? [this drawable]
    (satisfies? drawable/TriangleListDrawable drawable))

  (draw-drawables [this drawables gl]
    (let [{:keys [width height]} (opengl/size gl)
          [coordinates colors] (loop [coordinates []
                                      colors []
                                      drawables drawables]
                                 (if-let [drawable (first drawables)]
                                   (let [[new-coordinates new-colors] (drawable/triangles drawable)]
                                     (recur (concat coordinates new-coordinates)
                                            (concat colors new-colors)
                                            (rest drawables)))
                                   [coordinates colors]))]

      (assoc this :triangle-list
             (-> triangle-list
                 (multicolor-triangle-list/set-size width height gl)
                 (multicolor-triangle-list/render-coordinates coordinates
                                                              colors
                                                              gl)))))

  (start-frame [this gl] this)

  (end-frame [this gl] this)

  (delete [this gl] this))

(defn create-triangle-list-renderer [gl]
  (->TriangleListRenderer (multicolor-triangle-list/create gl :triangles)))

(defrecord QuadViewRenderer [quad-view]
  Renderer
  (can-draw? [this drawable]
    (or (satisfies? drawable/Java2DDrawable drawable)
        (:has-predefined-texture drawable)))

  (draw-drawables [this drawables gl]
    (do (doto gl
          #_(.glEnable GL2/GL_BLEND)
          #_(.glBlendFuncSeparate GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA GL2/GL_ONE GL2/GL_ONE)
          #_(.glBlendFunc GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA))
        (let [{:keys [width height]} (opengl/size gl)]
          (assoc this :quad-view (quad-view/draw-drawables quad-view drawables width height gl)))))

  (start-frame [this gl]
    this)

  (end-frame [this gl]
    (let [frames-since-garbage-collection (or (:frames-since-garbage-collection this)
                                              0)]
      (if (> frames-since-garbage-collection
             100)
        (assoc this
               :quad-view (quad-view/unload-unused-textures quad-view)
               :frames-since-garbage-collection 0)

        (assoc this
               :frames-since-garbage-collection (inc frames-since-garbage-collection)))))

  (delete [this gl]
    this))

(defn create-quad-view-renderer [gl]
  (->QuadViewRenderer (quad-view/create gl)))

(defrecord QuadRenderer [quad programs]
  Renderer
  (can-draw? [this drawable]
    (instance? Quad  drawable))

  (draw-drawables [this drawables gl]
    (let [viewport-size (opengl/size gl)]
      (loop [this this
             drawables drawables]
        (if-let [drawable (first drawables)]
          
          (let [program (or (get programs (:fragment-shader-source drawable))
                            (quad/create-program quad (:fragment-shader-source drawable) gl))]
            
            (quad/draw gl
                       (:textures drawable)
                       (:uniforms drawable)
                       program
                       (:x drawable) (:y drawable)
                       (:width drawable) (:height drawable)
                       (:width viewport-size) (:height viewport-size))
            (recur (-> this
                       (update-in [:programs] assoc (:fragment-shader-source drawable) program)
                       (update-in [:used-fragment-shader-sources] conj (:fragment-shader-source drawable)))
                   (rest drawables)))
          
          
          this))))

  (start-frame [this gl]
    (assoc this :used-fragment-shader-sources #{}))

  (end-frame [this gl]
    (assoc this :programs (reduce (fn [programs fragment-shader-source]
                                    (shader/delete-program gl (get programs fragment-shader-source))
                                    (dissoc programs fragment-shader-source))
                                  (:programs this)
                                  (filter (complement (:used-fragment-shader-sources this))
                                          (keys (:programs this))))))

  (delete [this gl]
    (quad/delete quad gl)
    this))

(defn create-quad-renderer [gl]
  (->QuadRenderer (quad/create gl)
                  {}))

(defrecord RenderTargetRenderer [renderers]
  Renderer
  (can-draw? [this drawable]
    (:render-target? drawable))

  (draw-drawables [this render-target-drawables gl]
    (assoc this :renderers
           (reduce (fn [renderers render-target-drawable]
                     (let [render-target (render-target/create (:width render-target-drawable)
                                                               (:height render-target-drawable)
                                                               gl)]

                       (let [renderers (render-target/render-to render-target gl
                                                                (opengl/clear gl 0 0 0 1)
                                                                (render-frame-drawables (:child-drawables render-target-drawable)
                                                                                        gl
                                                                                        renderers))]
                         (let [{:keys [width height]} (opengl/size gl)]
                           (render-target/draw render-target (:x render-target-drawable) (:y render-target-drawable) width height gl))

                         (render-target/delete render-target gl)
                         renderers)))
                   renderers
                   render-target-drawables)))

  (start-frame [this gl]
    this)

  (end-frame [this gl]
    this)

  (delete [this gl]
    this))

(defn create-render-target-renderer [renderers gl]
  (let [render-target-renderer (->RenderTargetRenderer renderers)]
    (update-in render-target-renderer [:renderers]  conj render-target-renderer)))

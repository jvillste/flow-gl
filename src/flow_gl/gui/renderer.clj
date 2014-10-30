(ns flow-gl.gui.renderer
  (:require
   (flow-gl.gui [drawable :as drawable]
                [quad-view :as quad-view])

   (flow-gl.opengl.jogl [opengl :as opengl]
                        [render-target :as render-target]
                        [quad :as quad]
                        [shader :as shader]))
  (:import [nanovg NanoVG]
           [flow_gl.gui.drawable Quad]
           [javax.media.opengl GL2]))

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
  (first (filter #(can-draw? % drawable) renderers)))

(defn render-drawables-with-renderers [drawables gl renderers]
  (let [batches (group-by (partial select-renderer renderers) drawables)]
    (loop [renderers renderers
           rendered-renderers []]
      (if-let [renderer (first renderers)]
        (recur (rest renderers)
               (conj rendered-renderers
                     (draw-drawables renderer
                                     (or (get batches renderer)
                                         [])
                                     gl)))
        rendered-renderers))))

(defn render-layers [layers gl renderers]
  (loop [renderers renderers
         layers layers]
    (if-let [layer-drawables (first layers)]
      (recur (render-drawables-with-renderers layer-drawables gl renderers)
             (rest layers))
      renderers)))

(defn create-layers [drawables]
  (->> drawables
       (sort-by :z)
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


(defrecord NanoVGRenderer [nanovg]
  Renderer
  (can-draw? [this drawable]
    (satisfies? drawable/NanoVGDrawable drawable))

  (draw-drawables [this drawables gl]
    (let [{:keys [width height]} (opengl/size gl)]
      (NanoVG/beginFrame nanovg width height)
      (doseq [drawable drawables]
        (NanoVG/resetTransform nanovg)
        (NanoVG/translate nanovg
                          (:x drawable)
                          (:y drawable))
        (drawable/draw-nanovg drawable nanovg))
      (NanoVG/endFrame nanovg))
    this)

  (start-frame [this gl] this)

  (end-frame [this gl] this)

  (delete [this gl] this))

(defn create-nanovg-renderer []
  (->NanoVGRenderer (NanoVG/init)))

(defrecord GLRenderer []
  Renderer
  (can-draw? [this drawable]
    (satisfies? drawable/GLDrawable drawable))

  (draw-drawables [this drawables gl]
    (doseq [drawable drawables]
      (drawable/draw-gl drawable gl))
    this)

  (start-frame [this gl] this)

  (end-frame [this gl] this)

  (delete [this gl] this))

(defn create-gl-renderer []
  (->GLRenderer))

(defrecord QuadViewRenderer [quad-view]
  Renderer
  (can-draw? [this drawable]
    (satisfies? drawable/Java2DDrawable drawable))

  (draw-drawables [this drawables gl]
    ;;(println "drawing " (-> quad-view :quad-batch :texture-buffer-id ) drawables)
    (doto gl
      (.glEnable GL2/GL_BLEND)
      (.glBlendFunc GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA))
    (let [{:keys [width height]} (opengl/size gl)]
      (assoc this :quad-view (quad-view/draw-drawables quad-view drawables width height gl))))

  (start-frame [this gl]
    this)

  (end-frame [this gl]
    ;;(println "end frame " (-> quad-view :quad-batch :quad-parameters-buffer-id ))
    (assoc this :quad-view (quad-view/unload-unused-textures quad-view)))

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

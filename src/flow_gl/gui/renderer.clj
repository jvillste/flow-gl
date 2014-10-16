(ns flow-gl.gui.renderer
  (:require
   (flow-gl.gui [drawable :as drawable]
                [quad-view :as quad-view])

   (flow-gl.opengl.jogl [opengl :as opengl]))
  (:import [nanovg NanoVG]
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

(defrecord QuadViewRenderer [quad-view]
  Renderer
  (can-draw? [this drawable]
    (satisfies? drawable/Java2DDrawable drawable))

  (draw-drawables [this drawables gl]
    (doto gl
      (.glEnable GL2/GL_BLEND)
      (.glBlendFunc GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA))
    (let [{:keys [width height]} (opengl/size gl)]
      (assoc this :quad-view (quad-view/draw-drawables quad-view drawables width height gl))))

  (start-frame [this gl]
    this)

  (end-frame [this gl]
    (assoc this :quad-view (quad-view/unload-unused-textures quad-view)))

  (delete [this gl]
    this))

(defn create-quad-view-renderer [gl]
  (->QuadViewRenderer (quad-view/create gl)))

(defrecord RenderTargetRenderer [renderers]
  Renderer
  (can-draw? [this drawable]
    (:render-target? drawable))

  (draw-drawables [this render-targets gl]
    (assoc this :renderers
           (reduce (fn [renderers render-target-drawable]
                     #_(render-frame-drawables (:child-drawables render-target-drawable)
                                             gl
                                             renderers)
                     (let [render-target (render-target/create (:width render-target-drawable)
                                                               (:height render-target-drawable)
                                                               gl)]

                       

                       (render-target/start-rendering render-target gl)

                       (render-frame-drawables (:child-drawables render-target-drawable)
                                               gl
                                               renderers)
                       (opengl/clear gl 1 0 0 1)
                       (render-target/end-rendering render-target gl)

                       (let [{:keys [width height]} (opengl/size gl)]
                         (render-target/draw render-target width height gl))

                       (render-target/delete render-target gl)))
                   renderers
                   render-targets)))

  (start-frame [this gl]
    this)

  (end-frame [this gl]
    this)

  (delete [this gl]
    this))

(defn create-render-target-renderer [renderers gl]
  (let [render-target-renderer (->RenderTargetRenderer renderers)]
    (update-in render-target-renderer [:renderers]  conj render-target-renderer)))

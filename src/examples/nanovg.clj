(ns examples.nanovg
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
            (flow-gl.graphics [font :as font])
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defprotocol Renderer
  (can-draw? [this drawable])

  (draw-drawables [this drawables gl])

  (start-frame [this gl])

  (end-frame [this gl])

  (delete [this gl]))

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

(defn drawables-for-layout
  ([layout]
     (drawables-for-layout layout 0 0 0 []))

  ([layout parent-x parent-y parent-z quads]
     (if (:children layout)
       (let [parent-x (+ parent-x (:x layout))
             parent-y (+ parent-y (:y layout))
             parent-z (+ parent-z (or (:z layout) 0))]
         (loop [quads quads
                children (:children layout)]
           (if-let [child (first children)]
             (let [quads (drawables-for-layout child parent-x parent-y parent-z quads)]
               (recur quads
                      (rest children)))
             quads)))
       (conj quads
             (assoc layout
               :x (+ parent-x (:x layout))
               :y (+ parent-y (:y layout))
               :z (+ parent-z (or (:z layout) 0)))))))


(deftest drawables-for-layout-test
  (let [result (let [[state layout] (layout/layout (layouts/->HorizontalStack [(assoc (layouts/->VerticalStack [(drawable/->Text "Foo1"
                                                                                                                                 (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                                 [1 1 1 1])
                                                                                                                (assoc (drawable/->Text "Bar1"
                                                                                                                                        (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                                        [1 1 1 1])
                                                                                                                  :state-path-part [:bar] )])
                                                                                 :state-path-part [:child-states 0]
                                                                                 :z 1)
                                                                               (assoc (layouts/->VerticalStack [(drawable/->Text "Foo2"
                                                                                                                                 (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                                 [1 1 1 1])
                                                                                                                (drawable/->Text "Bar2"
                                                                                                                                 (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                                 [1 1 1 1])])
                                                                                 :state-path-part [:child-states 1])] )
                                                   {}
                                                   100 100)]
                 (drawables-for-layout (assoc layout :x 0 :y 0)))]
    (is (= result
           nil))))

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

(defn render-frame [drawables gl renderers]
  (->> renderers
       (map-for-renderers start-frame gl)
       (render-layers (->> drawables
                           (sort-by :z)
                           (partition-by :z))
                      gl)
       (map-for-renderers end-frame gl)))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 5]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn start-view [drawables-for-time]
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :close-automatically true)]


    (try
      (let [renderers-atom (atom (window/with-gl window gl [#_(create-nanovg-renderer)
                                                            (create-quad-view-renderer gl)]))]
        (loop []
          (let [frame-started (System/currentTimeMillis)]
            (let [drawables (drawables-for-time frame-started)]
              (window/set-display window gl
                                  (opengl/clear gl 0 0 0 1)
                                  (reset! renderers-atom
                                          (render-frame drawables gl @renderers-atom))))

            (when (window/visible? window)
              (do (wait-for-next-frame frame-started)
                  (recur))))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn drawables-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]
    [(assoc (drawable/->Rectangle 100 100 [255 0 0 255])
       :x (* 100 phase)
       :y 0)
     (let [text (drawable/->Text "Foo"
                                 (font/create "LiberationSans-Regular.ttf" 14)
                                 [1 1 1 1])
           preferred-size (layoutable/preferred-size text 1000 1000)]
       (assoc text
         :width (:width preferred-size)
         :height (:height preferred-size)
         :x (* 100 phase)
         :y 0))
     ]))

(defn start []
  (start-view drawables-for-time))

(run-tests)

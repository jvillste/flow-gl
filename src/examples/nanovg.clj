(ns examples.nanovg
  (:require [flow-gl.gui.layouts :as layouts]
            [flow-gl.gui.layout :as layout]
            [flow-gl.gui.drawable :as drawable]
            [clojure.data.priority-map :as priority-map]
            [flow-gl.opengl.jogl.opengl :as opengl]
            (flow-gl.graphics [font :as font]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defprotocol Renderer
  (add-drawable [this drawable])

  (start-frame [this gl])
  (end-frame [this gl])

  (start-batch [this gl])
  (end-batch [this gl])

  (delete [this gl]))

(defrecord NanoVGRenderer []
  Renderer
  
  (add-drawable [this drawable]
    (if (satisfies? drawable/NanoVGDrawable drawable)
      [(update-in this [:drawables] conj drawable)
       true]
      [this
       false]))

  (start-batch [this gl] (assoc this :drawables []))
  
  (end-batch [this gl]
    (let [nanovg (:nanovg this)
          {:keys [width height]} (opengl/size gl)]
      (NanoVG/beginFrame nanovg width height)
      (doseq [drawable (:drawables this)]
        (drawable/draw-nanovg drawable nanovg))
      (NanoVG/endFrame nanovg)))

  (start-frame [this gl] this)

  (end-frame [this gl] this))

(defn create-nanovg-renderer []
  {:drawables []
   :nanovg (NanoVG/init)})

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

(defn add-drawable [drawable renderers]
  (loop [renderers renderers
         tried-renderers []]
    (if-let [renderer (first renderers)]
      (let [[renderer accepted] (add-drawable renderer drawable)]
        (if accepted
          (concat tried-renderers [renderer] (rest renderers))
          (recur (rest renderers)
                 (conj tried-renderers renderer)))))))

(defn add-drawables [drawables renderers]
  (loop [drawables drawables
         renderers renderers]
    (if-let [drawable (first drawables)]
      (recur (rest drawables)
             (add-drawable drawable renderers))
      renderers)))

(defn render-layers [layers gl renderers]
  (loop [renderers renderers
         layers layers]
    (if-let [layer-drawables (first layers)]
      (recur (->> renderers
                  (map-for-renderers start-batch gl)
                  (add-drawables layer-drawables)
                  (map-for-renderers end-batch gl))
             (rest layers))
      renderers)))

(defn render-frame [drawables gl renderers]
  (->> renderers
       (map-for-renderers start-frame gl renderers)
       (render-layers (->> drawables
                           (sort-by :z)
                           (partition-by :z))
                      gl)
       (map-for-renderers end-frame gl)))



(run-tests)

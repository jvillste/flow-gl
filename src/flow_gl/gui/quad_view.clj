(ns flow-gl.gui.quad-view
  (:require (flow-gl.gui [event-queue :as event-queue]
                         [layout :as layout]
                         [drawable :as drawable]
                         [layoutable :as layoutable])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture]
                                 [shader :as shader]
                                 [buffer :as buffer]
                                 [quad-batch :as quad-batch])
            [flow-gl.opengl.math :as math]
            [clojure.data.priority-map :as priority-map]
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]
                              [native-buffer :as native-buffer]))
  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color])

  (:use clojure.test))

#_(defn quads-for-layout
  ([layout]
     (quads-for-layout layout -1 0))

  ([layout parent next-free-id]
     (if (satisfies? drawable/Java2DDrawable layout)
       [{:drawable (dissoc layout :x :y)
         :x (:x layout)
         :y (:y layout)
         :parent parent}]
       (let [current-id next-free-id]
         (loop [quads [{:x (:x layout)
                        :y (:y layout)
                        :parent parent}]
                children (:children layout)
                next-free-id (inc next-free-id)]
           (if-let [child (first children)]
             (let [child-quads (quads-for-layout child current-id next-free-id)]
               (recur (concat quads
                              child-quads)
                      (rest children)
                      (+ next-free-id (count child-quads))))
             quads))))))

(defn compare-z-and-index [[z-1 index-1] [z-2 index-2]]
  (let [result (compare z-1 z-2)]
    (if (= result 0)
      (compare index-1 index-2)
      result)))

(defn quads-for-layout
  ([layout]
     (keys (second (quads-for-layout layout 0 0 0 0 (priority-map/priority-map-by compare-z-and-index)))))

  ([layout index parent-x parent-y parent-z quads]
     (if (satisfies? drawable/Java2DDrawable layout)
       [(inc index)
        (assoc quads
          {:drawable (dissoc layout :x :y)
           :x (+ parent-x (:x layout))
           :y (+ parent-y (:y layout))}
          [(+ parent-z (or (:z layout) 0)) index])]
       (let [parent-x (+ parent-x (:x layout))
             parent-y (+ parent-y (:y layout))
             parent-z (+ parent-z (or (:z layout) 0))]
         (loop [quads quads
                index index
                children (:children layout)]
           (if-let [child (first children)]
             (let [[index quads] (quads-for-layout child index parent-x parent-y parent-z quads)]
               (recur quads
                      index
                      (rest children)))
             [index quads]))))))

(deftest quads-for-layout-test
  (is (=  (let [layout (layout/layout (layout/->HorizontalStack [(assoc (layout/->VerticalStack [(drawable/->Text "Foo1"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])
                                                                                                 (assoc (drawable/->Text "Bar1"
                                                                                                                         (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                         [1 1 1 1])
                                                                                                   :state-path-part [:bar] )])
                                                                   :state-path-part [:child-states 0]
                                                                   :z 1)
                                                                 (assoc (layout/->VerticalStack [(drawable/->Text "Foo2"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])
                                                                                                 (drawable/->Text "Bar2"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])])
                                                                   :state-path-part [:child-states 1])] )
                                      100 100)]
            (seq (quads-for-layout (assoc layout :x 0 :y 0))))

          nil)))

(defn unused-drawable-textures [drawable-textures quads]
  (->> quads
       (map :drawable)
       (reduce dissoc drawable-textures)))

(defn new-drawables [drawable-textures quads]
  (reduce (fn [new-drawables quad]
            (if (contains? quad :drawable)
              (if (contains? drawable-textures (:drawable quad))
                new-drawables
                (conj new-drawables (:drawable quad)))
              new-drawables ))
          #{}
          quads))

(defn create-textures [drawables]
  (map (fn [drawable]
         (let [buffered-image (buffered-image/create (max 1 (:width drawable))
                                                     (max 1 (:height drawable)))]
           (drawable/draw drawable
                          (buffered-image/get-graphics buffered-image))
           buffered-image))
       drawables))

(defn add-new-textures [drawable-textures drawables first-texture-id]
  (loop [texture-id first-texture-id
         drawable-textures drawable-textures
         drawables drawables]
    (if-let [drawable (first drawables)]
      (recur (inc texture-id)
             (assoc drawable-textures drawable texture-id)
             (rest drawables))
      drawable-textures)))

(defn load-new-textures [quad-view quads gl]
  (let [first-texture-id (:next-free-texture-id (:quad-batch quad-view))
        drawables (new-drawables (:drawable-textures quad-view) quads)
        new-textures (create-textures drawables)]
    (if (empty? new-textures)
      quad-view
      (assoc quad-view
        :quad-batch (quad-batch/add-textures (:quad-batch quad-view) gl new-textures)
        :drawable-textures (add-new-textures (:drawable-textures quad-view) drawables (:next-free-texture-id (:quad-batch quad-view)))))))

(defn add-texture-ids [quads drawable-textures]
  (map (fn [quad]
         (if (contains? quad :drawable)
           (assoc quad
             :texture-id (get drawable-textures
                              (:drawable quad)))

           quad))
       quads))

(defn unload-unused-textures [quad-view quads]
  (let [unused-drawable-textures (unused-drawable-textures (:drawable-textures quad-view)
                                                           quads)
        new-quad-batch (reduce quad-batch/remove-texture
                               (:quad-batch quad-view)
                               (vals unused-drawable-textures))
        new-drawable-textures (apply dissoc
                                     (:drawable-textures quad-view)
                                     (keys unused-drawable-textures))]

    (assoc quad-view
      :quad-batch new-quad-batch
      :drawable-textures new-drawable-textures)))

(defn draw-quads [quad-view quads width height gl]
  #_(println (select-keys (:quad-batch quad-view)
                          [:allocated-texels
                           :next-free-texel
                           :removed-texels]))
  (let [quad-view (load-new-textures quad-view
                                     quads
                                     gl)
        quad-view (unload-unused-textures quad-view quads)
        quads (add-texture-ids quads
                               (:drawable-textures quad-view))]
    (assoc quad-view :quad-batch
           (quad-batch/draw-quads (:quad-batch quad-view)
                                  gl
                                  quads
                                  width height))))

(defn draw-layout [quad-view layout width height gl]
  (draw-quads quad-view
              (quads-for-layout (assoc layout :x 0 :y 0))
              width
              height
              gl))

(defn create [gl]
  {:drawable-textures {}
   :quad-batch (quad-batch/create gl)})

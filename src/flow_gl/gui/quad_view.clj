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


(defn quads-for-layout
  ([layout]
     (keys (quads-for-layout layout 0 0 0 (priority-map/priority-map ))))

  ([layout parent-x parent-y parent-z quads]
     (if (satisfies? drawable/Java2DDrawable layout)
       (assoc quads
         {:drawable (dissoc layout :x :y)
          :x (+ parent-x (:x layout))
          :y (+ parent-y (:y layout))}
         (+ parent-z (or (:z layout) 0)))
       (let [parent-x (+ parent-x (:x layout))
             parent-y (+ parent-y (:y layout))
             parent-z (+ parent-z (or (:z layout) 0))]
         (loop [quads quads
                children (:children layout)]
           (if-let [child (first children)]
             (let [quads (quads-for-layout child parent-x parent-y parent-z quads)]
               (recur quads
                      (rest children)))
             quads))))))

#_(deftest quads-for-layout-test
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

(defn texture-key [drawable]
  (dissoc drawable :x :y :z :has-predefined-texture #_:texture-id))

(defn unused-drawable-textures [drawable-textures drawables]
  (reduce dissoc drawable-textures (map texture-key drawables)))

(defn has-texture? [quad-view drawable]
  (contains? (:drawable-textures quad-view) (texture-key drawable)))

(defn set-texture [drawable-textures drawable texture-id]
  (assoc drawable-textures (texture-key drawable) texture-id))

(defn unset-texture [drawable-textures drawable]
  (dissoc drawable-textures (texture-key drawable)))

(defn texture [drawable-textures drawable]
  (get drawable-textures (texture-key drawable)))

(defn new-drawables [quad-view drawables]
  (->> drawables
       (filter #(not (has-texture? quad-view %)))
       (apply hash-set)))

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
             (set-texture drawable-textures drawable texture-id)
             (rest drawables))
      drawable-textures)))

(defn load-new-textures [quad-view drawables gl]
  (let [first-texture-id (:next-free-texture-id (:quad-batch quad-view))
        drawables (new-drawables quad-view drawables)
        new-textures (create-textures drawables)]
    
    (if (empty? new-textures)
      quad-view
      (assoc quad-view
        :quad-batch (quad-batch/add-textures (:quad-batch quad-view) gl new-textures)
        :drawable-textures (add-new-textures (:drawable-textures quad-view)
                                             drawables
                                             (:next-free-texture-id (:quad-batch quad-view)))))))

(defn add-gl-texture [quad-view drawable texture-id width height gl]
  (assoc quad-view
    :quad-batch (quad-batch/add-textures-from-gl-textures (:quad-batch quad-view)
                                                          gl
                                                          [{:texture-id texture-id
                                                            :width (int width)
                                                            :height (int height)}])
    :drawable-textures (add-new-textures (:drawable-textures quad-view)
                                         [drawable]
                                         (:next-free-texture-id (:quad-batch quad-view)))))

(defn add-texture-ids [drawables drawable-textures]
  (map (fn [drawable]
         (assoc drawable
           :texture-id (texture drawable-textures
                                drawable)))
       drawables))

(defn unload-unused-textures [quad-view]
  (let [unused-drawable-textures (unused-drawable-textures (:drawable-textures quad-view)
                                                           (:drawn-drawables quad-view))
        new-quad-batch (reduce quad-batch/remove-texture
                               (:quad-batch quad-view)
                               (vals unused-drawable-textures))
        new-drawable-textures (reduce unset-texture
                                      (:drawable-textures quad-view)
                                      (keys unused-drawable-textures))]
    (assoc quad-view
      :drawn-drawables []
      :quad-batch new-quad-batch
      :drawable-textures new-drawable-textures)))

(defn draw-drawables [quad-view drawables width height gl]
  (let [quad-view (load-new-textures quad-view
                                     drawables
                                     gl)]

    (assoc quad-view
      :drawn-drawables (concat (:drawn-drawables quad-view)
                               drawables)

      :quad-batch (quad-batch/draw-quads (:quad-batch quad-view)
                                         gl
                                         (add-texture-ids drawables
                                                          (:drawable-textures quad-view))
                                         width height))))



#_(defn draw-layout [quad-view layout width height gl]
    (flow-gl.debug/debug-timed-and-return "draw-quads" (draw-quads quad-view
                                                                   (flow-gl.debug/debug-timed-and-return "quad-for-layout" (quads-for-layout (assoc layout :x 0 :y 0)))
                                                                   width
                                                                   height
                                                                   gl)))

(defn create [gl]
  {:drawable-textures {}
   :drawn-drawables []
   :quad-batch (quad-batch/create gl)})

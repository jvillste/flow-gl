(ns examples.batches
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
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]
                              [native-buffer :as native-buffer]))
  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color])

  (:use clojure.test))

#_(defprotocol BatchItem
    (add [drawable gl layer parents batches])
    (remove [drawable gl layer parents batches]))

(defn wait-for-frame-end-time [frame-start-time framerate]
  (let [frame-length (/ 1E9
                        framerate)
        time-spent-until-now (- (System/nanoTime)
                                frame-start-time)]

    (Thread/sleep (max 0
                       (/ (- frame-length
                             time-spent-until-now)
                          1000000)))))



(defn text-image [text]
  (text/create-buffered-image [1 1 1 1]
                              (font/create "LiberationSans-Regular.ttf" 20)
                              text))

(defn image-for-java2d-drawable [drawable]
  )

#_(defn quads-for-layout [layout state-path parent next-free-id]
    (let [this-layout-id next-free-id]
      (loop [quads [{:drawable (buffered-image/create 1 1)
                     :x (:x layout)
                     :y (:y layout)
                     :parent parent}]
             children (:children layout)
             next-free-id (inc next-free-id)
             state-path-quads {}]

        (if-let [child (first children)]
          (let [state-path (if-let [state-path-part (:state-path-part child)]
                             (concat state-path state-path-part)
                             state-path)
                state-path-quads (if-let [state-path-part (:state-path-part child)]
                                   (assoc state-path-quads state-path next-free-id)
                                   state-path-quads)]
            (if (satisfies? drawable/Java2DDrawable child)
              (recur (concat quads
                             [{:drawable (let [image (buffered-image/create (:width child)
                                                                            (:height child))]
                                           (drawable/draw child (buffered-image/get-graphics image))
                                           image)
                               :x (:x child)
                               :y (:y child)
                               :parent this-layout-id}])
                     (rest children)
                     (inc next-free-id)
                     state-path-quads)
              (let [[layout-quads child-state-path-quads] (quads-for-layout child state-path this-layout-id next-free-id)]
                (recur (concat quads
                               layout-quads)
                       (rest children)
                       (+ next-free-id (count layout-quads))
                       (conj state-path-quads child-state-path-quads)))))

          [quads state-path-quads]))))

(defn quads-for-layout
  ([layout]
     (quads-for-layout layout -1 0))

  ([layout parent next-free-id]
     (if (satisfies? drawable/Java2DDrawable layout)
       [{:drawable (dissoc layout :x :y)
         :x (:x layout)
         :y (:y layout)
         :parent parent}]
       (let [curretn-id next-free-id]
         (loop [quads [{:x (:x layout)
                        :y (:y layout)
                        :parent parent}]
                children (:children layout)
                next-free-id (inc next-free-id)]
           (if-let [child (first children)]
             (let [child-quads (quads-for-layout child curretn-id next-free-id)]
               (recur (concat quads
                              child-quads)
                      (rest children)
                      (+ next-free-id (count child-quads))))
             quads))))))

(deftest quads-for-layout-test
  (is (=  (let [layout (layout/layout (layout/->HorizontalStack [(assoc (layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])
                                                                                                 (assoc (drawable/->Text "Bar"
                                                                                                                         (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                         [1 1 1 1])
                                                                                                   :state-path-part [:bar] )])
                                                                   :state-path-part [:child-states 0])
                                                                 (assoc (layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])
                                                                                                 (drawable/->Text "Bar"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])])
                                                                   :state-path-part [:child-states 1])] )
                                      100 100)]
            (quads-for-layout (assoc layout :x 0 :y 0)
                              -1
                              0))

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
         (let [buffered-image (buffered-image/create (:width drawable)
                                                     (:height drawable))]
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
    (quad-batch/draw-quads (:quad-batch quad-view)
                           gl
                           quads
                           width height)
    quad-view))

(defn create-quad-view [gl]
  {:drawable-textures {}
   :quad-batch (quad-batch/create gl)})

(defn render-loop [quad-view window]
  (loop [quad-view quad-view
         frame-start-time (System/nanoTime)]
    (let [phase (/ (mod (System/nanoTime)
                        (* 3 1E9))
                   (* 3 1E9))
          quads (quads-for-layout (assoc (layout/layout (layout/->HorizontalStack [(layout/->VerticalStack [(drawable/->Text (str phase)
                                                                                                                             (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                             [1 1 1 1])
                                                                                                            (layout/->Margin (* phase 50 ) 0 0 0
                                                                                                                             [(drawable/->Text "Bar"
                                                                                                                                               (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                                               [1 1 1 1])])])
                                                                                   (layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                                                             (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                             [1 1 1 1])
                                                                                                            (drawable/->Text "Bar"
                                                                                                                             (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                             [1 1 1 1])])])
                                                        (window/width window)
                                                        (window/height window))
                                    :x 0 :y 0))
          new-quad-view (window/with-gl window gl
                          (opengl/clear gl 0 0 0 1)
                          (draw-quads quad-view
                                      quads
                                      (window/width window)
                                      (window/height window)
                                      gl))]

      (wait-for-frame-end-time frame-start-time 60)
      (when (.isVisible (:gl-window window))
        (recur new-quad-view
               (System/nanoTime))))))

(defn start []
  (let [width 500
        height 500
        window (window/create width height :profile :gl3)]

    (try
      (let [quad-view (window/with-gl window gl
                        (opengl/initialize gl)
                        (create-quad-view gl))]
        (render-loop quad-view window))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))

;; TODO
;; optimize updating the same quads constantly. generational GC?
;; share texture
;; group quads to tiles and draw given tiles only
;; load new quads asynchronously in small batches. Ready made byte buffers to be loaded to the GPU

(run-tests)

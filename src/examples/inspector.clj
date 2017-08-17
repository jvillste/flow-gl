(ns examples.inspector
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

  (:import [com.jogamp.opengl GL2]
           [java.io PrintWriter StringWriter]
           [java.nio IntBuffer]
           [java.awt Color])

  (:use clojure.test))

(defn quads-for-layout [layout parent next-free-id]
  (let [this-layout-id next-free-id]
    (loop [quads [{:image (buffered-image/create 1 1)
                   :x (:x layout)
                   :y (:y layout)
                   :parent parent}]
           children (:children layout)
           next-free-id (inc next-free-id)]

      (if-let [child (first children)]
        (if (satisfies? drawable/Java2DDrawable child)
          (recur (concat quads
                         [{:image (let [image (buffered-image/create (:width child)
                                                                     (:height child))]
                                    (drawable/draw child (buffered-image/get-graphics image))
                                    image)
                           :x (:x child)
                           :y (:y child)
                           :parent this-layout-id}])
                 (rest children)
                 (inc next-free-id))
          (let [layout-quads (quads-for-layout child this-layout-id next-free-id)]
            (recur (concat quads
                           layout-quads)
                   (rest children)
                   (+ next-free-id (count layout-quads)))))
        quads))))

(deftest quads-for-layout-test
  (is (=  (let [layout (layout/layout (layout/->HorizontalStack [(layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                                           (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                           [1 1 1 1])
                                                                                          (drawable/->Text "Bar"
                                                                                                           (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                           [1 1 1 1])])
                                                                 (layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                                           (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                           [1 1 1 1])
                                                                                          (drawable/->Text "Bar"
                                                                                                           (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                           [1 1 1 1])])] )
                                      100 100)]
            (println "layout " layout)
            (quads-for-layout (assoc layout :x 0 :y 0)
                              -1
                              0))

          nil)))

(defn show-layoutable [layoutable]
  (let [width 500
        height 500
        window (window/create width height :profile :gl3)]

    (try
      (window/with-gl window gl
        (opengl/initialize gl)

        (-> (quad-batch/create gl)
            (quad-batch/add-quads gl
                                  (-> layoutable
                                      (layout/layout width height)
                                      (assoc :x 0 :y 0)
                                      (quads-for-layout -1 0)))
            (quad-batch/draw gl
                             width
                             height)))


      (catch Exception e
        (window/close window)
        (throw e)))))

(defn start []
  (show-layoutable (layout/->HorizontalStack [(layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                    (font/create "LiberationSans-Regular.ttf" 14)
                                                                                    [1 1 1 1])
                                                                   (drawable/->Text "Bar"
                                                                                    (font/create "LiberationSans-Regular.ttf" 14)
                                                                                    [1 1 1 1])])
                                          (layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                    (font/create "LiberationSans-Regular.ttf" 14)
                                                                                    [1 1 1 1])
                                                                   (drawable/->Text "Bar"
                                                                                    (font/create "LiberationSans-Regular.ttf" 14)
                                                                                    [1 1 1 1])])])))

;; TODO
;; optimize updating the same quads constantly. generational GC?
;; share texture
;; group quads to tiles and draw given tiles only
;; load new quads asynchronously in small batches. Ready made byte buffers to be loaded to the GPU

(run-tests)

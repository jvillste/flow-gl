(ns examples.batches
  (:require (flow-gl.gui [event-queue :as event-queue]
                         [layout :as layout]
                         [drawable :as drawable]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view])
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


(defn wait-for-frame-end-time [frame-start-time framerate]
  (let [frame-length (/ 1E9
                        framerate)
        time-spent-until-now (- (System/nanoTime)
                                frame-start-time)]

    (Thread/sleep (max 0
                       (/ (- frame-length
                             time-spent-until-now)
                          1000000)))))


(defn render-loop [quad-view window]
  (loop [quad-view quad-view
         frame-start-time (System/nanoTime)]
    (let [phase (/ (mod (System/nanoTime)
                        (* 3 1E9))
                   (* 3 1E9))
          new-quad-view (window/with-gl window gl
                          (opengl/clear gl 0 0 0 1)
                          (quad-view/draw-layout  quad-view
                                                  (assoc (layout/layout (layout/->HorizontalStack [(layout/->VerticalStack [(drawable/->Text (str phase)
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
                                                    :x 0 :y 0)
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
                        (quad-view/create gl))]
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

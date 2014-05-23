(ns examples.batches
  (:require [flow-gl.gui.event-queue :as event-queue]
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
           [java.awt Color]))


(defprotocol BatchItem
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

(defn render-loop [quad-batch window]
  (loop [quad-batch quad-batch
         frame-start-time (System/nanoTime)]
    (let [new-quad-batch (window/with-gl window gl
                           (opengl/clear gl 0 0 0 1)
                           (-> quad-batch
                               (quad-batch/move-quad gl
                                                     0
                                                     (* (/ (mod (System/nanoTime)
                                                                (* 10 1E9))
                                                           (* 10 1E9))
                                                        400)
                                                     0)
                               (quad-batch/draw gl
                                                (window/width window)
                                                (window/height window))))]

      (wait-for-frame-end-time frame-start-time 60)
      (when (.isVisible (:gl-window window))
        (recur new-quad-batch
               (System/nanoTime))))))

(defn text-image [text]
  (text/create-buffered-image [1 1 1 1]
                              (font/create "LiberationSans-Regular.ttf" 14)
                              text))

(defn add-test-quads-batch [quad-batch gl]
  (-> quad-batch

      (quad-batch/add-quads gl
                            [{:image (text-image "parent")
                              :x 10
                              :y 30
                              :parent -1}])

      #_(quad-batch/add-quads gl
                              (let [per-column 25
                                    column-width 40
                                    row-height 16]
                                (for [number (range 300)]
                                  {:image (text-image (str number))
                                   :x (int (* (Math/floor (/ number per-column)) column-width))
                                   :y (int (+ 20 (* (mod number per-column) row-height)))
                                   :parent 0})))
      #_(quad-batch/remove-quad gl 3)
      (as-> quad-batch
            (do (println "before" quad-batch)
                quad-batch))
      #_(quad-batch/change-texture gl 0 (text-image "par"))
      #_(quad-batch/collect-garbage gl)
      (as-> quad-batch
            (do (println "after" quad-batch)
                quad-batch))))

(defn start []
  (let [width 500
        height 500
        ;;event-queue (event-queue/create)
        window (window/create width height :profile :gl3 #_:event-queue #_event-queue)
        images (map text-image ["for" "bar" "baz"])]

    (try
      (let [quad-batch (window/with-gl window gl
                         (opengl/initialize gl)

                         (-> (quad-batch/create gl)
                             (add-test-quads-batch gl)
                             (as-> quad-batch
                                   (do ;; (println quad-batch)
                                     ;;(println "quad index" (seq (buffer/read gl (:quad-index-buffer quad-batch) :int 0 (:next-free-quad quad-batch))))
                                     ;;(println "quad coordinate" (seq (buffer/read gl (:quad-coordinate-buffer-id quad-batch) :float 0 (* 2 (:next-free-quad quad-batch)))))
                                     ;;(println "texture offset" (seq (buffer/read gl (:texture-offset-attribute-buffer quad-batch) :int 0 (:next-free-quad quad-batch))))

                                     ;;(println "texture size" (seq (buffer/read gl (:texture-size-attribute-buffer quad-batch) :short 0 (* 2 (:next-free-quad quad-batch)))))
                                     quad-batch))
                             #_(draw gl width height)))]
        (render-loop quad-batch window))

      (println "exiting")

      (catch Exception e
        (window/close window)
        (throw e)))))

;; TODO
;; optimize updating the same quads constantly. generational GC?
;; share texture
;; group quads to tiles and draw given tiles only
;; load new quads asynchronously in small batches. Ready made byte buffers to be loaded to the GPU

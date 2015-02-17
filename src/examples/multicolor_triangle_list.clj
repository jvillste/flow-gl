(ns examples.multicolor-triangle-list
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [render-target :as render-target]
                                 [stencil :as stencil]
                                 [frame-buffer :as frame-buffer]
                                 [multicolor-triangle-list :as multicolor-triangle-list])
            [flow-gl.opengl.math :as math]
            [flow-gl.graphics.native-buffer :as native-buffer]
            (flow-gl.gui [window :as window]))
  (:use clojure.test)
  (:import [javax.media.opengl GL2]))

(defn quad [{:keys [x y width height]}]
  [x y
   x (+ y height)
   (+ x width) y

   x (+ y height)
   (+ x width) (+ y height)
   (+ x width) y])


(defn start-view []
  (let [window (jogl-window/create 800
                                   600
                                   :profile :gl3
                                   :init opengl/initialize
                                   :close-automatically true)]

    (let [triangle-list (window/with-gl window gl
                          (multicolor-triangle-list/create gl :triangles))

          rectangles (doall (for [x (range 20)
                                  y (range 20)]
                              {:x (* x 3)
                               :y (* y 3)
                               :width 2
                               :height 2}))
          coordinates (doall (map float (mapcat quad rectangles)))
          colors (doall (map float (apply concat (repeat (/ (count coordinates) 2) [1 0 1 1]))))]

      (try
        (window/with-gl window gl
          (let [{:keys [width height]} (opengl/size gl)]
            (opengl/clear gl 0 0 0 1)

            (multicolor-triangle-list/set-size triangle-list width height gl)
            (multicolor-triangle-list/render-coordinates triangle-list
                                                         (map float [0 0
                                                                     0 100
                                                                     100 100])
                                                         [1 0 0 1
                                                          1 0 0 1
                                                          1 0 0 1]
                                                         #_coordinates
                                                         #_colors
                                                         gl)))

        (window/swap-buffers window)

        (println "exiting")
        (catch Exception e
          (println "exception")
          (window/close window)
          (throw e))))))


(defn start []
  (start-view))

#_(run-tests)

#_(let [length 10000
      buffer ^java.nio.FloatBuffer (native-buffer/create-native-buffer :float (* 2 length))
      values (doall (repeat length (float 1)))]
  (time (doseq [^Float value values]
          (.put buffer #^floats (float-array [value value]))
          #_(.put buffer value))))

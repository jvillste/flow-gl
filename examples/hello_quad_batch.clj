(ns examples.hello-quad-batch
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            [flow-gl.debug :as debug]
            [flow-gl.csp :as csp]
            [taoensso.tufte :as timbre-profiling]
            [flow-gl.profiling :as profiling]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-view :as quad-view]
                         [window :as window]
                         [renderer :as renderer]
                         [layoutable :as layoutable])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]))
  (:use flow-gl.utils
        clojure.test))

(defn render-quads [window quad-batch quads]
  (let [quad-batch (window/with-gl window gl
                     (let [size (opengl/size gl)]
                       (opengl/clear gl 0 0 0 1)

                       (quad-batch/draw-quads quad-batch
                                              gl
                                              quads
                                              (:width size)
                                              (:height size))))]
    (taoensso.tufte/p :swap (window/swap-buffers window))
    quad-batch))


(defn set-size [drawable]
  (let [preferred-size (layoutable/preferred-size drawable 1000 1000)]
    (assoc drawable
           :width (:width preferred-size)
           :height (:height preferred-size))))

(defn text [text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 20)
                   [255 255 255 255]))

(defn initialize [quad-batch gl]
  #_(let [drawable (assoc
                    #_(set-size (text "FOO"))
                    (set-size (drawable/->Image (buffered-image/create-from-file "test.png")))
                    :x 0
                    :y 0)
          renderers [(renderer/create-quad-view-renderer gl)]
          render-target (render-target/create 100 100 #_(:width drawable)
                                              #_(:height drawable) gl)]

      (println (:width drawable)
               (:height drawable))

      (render-target/render-to render-target gl
                               (opengl/clear gl 0 0 0 1)
                               (renderer/render-frame [drawable]  gl renderers))

      (quad-batch/add-textures-from-gl-textures quad-batch gl [{:width (:width render-target)
                                                                :height (:height render-target)
                                                                :texture-id (:texture render-target)}]))

  (quad-batch/add-textures quad-batch gl [(buffered-image/create-from-file "pumpkin.png")]))

(defn quads [frame-time]
  #_[{:x 10
      :y 10
      :width 200
      :height 200
      :texture-id 0}]
  
  (doall (for [i (range 1 8000)]
           (let [round-time (* i 3000)]
             {:x (* (/ (mod frame-time round-time)
                       round-time)
                    100)
              :y (* i 1)
              :texture-id 0}))))

(time (do (quads 0)
          nil))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 1]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize
                                   :close-automatically true)]

    (try
      (loop [quad-batch (window/with-gl window gl
                          (-> (quad-batch/create gl)
                              (initialize gl)))]
        (let [frame-started (System/currentTimeMillis)
              quad-batch #_(timbre-profiling/profile :info :loop (render-quads window quad-batch (#'quads frame-started)))
              (time (render-quads window quad-batch (#'quads frame-started)))
              ]

          (when (window/visible? window)
            (do (wait-for-next-frame frame-started)
                (recur quad-batch)))))

      (println "exiting")
      (catch Exception e
        (println "exception" e)
        (window/close window)
        (throw e)))))

#_(profiling/profile-ns 'examples.hello-quad-batch)


(defn start []
  (.start (Thread. (fn [] (start-view)) )))

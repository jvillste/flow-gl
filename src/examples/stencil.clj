(ns examples.stencil
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [render-target :as render-target]
                                 [stencil :as stencil]
                                 [frame-buffer :as frame-buffer]
                                 [triangle-list :as triangle-list])
            (flow-gl.gui [window :as window]))
  (:use clojure.test)
  (:import [javax.media.opengl GL2]))


(defn start-view []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :close-automatically true)

        nanovg  (window/with-gl window gl
                  (NanoVG/init))]

    (let [triangle-list (window/with-gl window gl
                          (triangle-list/create gl :triangles))]
      (try
        (window/with-gl window gl
          (let [{:keys [width height]} (opengl/size gl)]
            (opengl/clear gl 0 0 0 1)

            (stencil/set [{:x 10 :y 10 :width 100 :height 100}
                          {:x 10 :y 120 :width 100 :height 100}]
                         gl)

            (triangle-list/set-size triangle-list width height gl)
            (triangle-list/render-coordinates triangle-list
                                              (quad {:x 0 :y 0 :width width :height height})
                                              [1 1 1 1]
                                              gl)

            (stencil/disable gl)))

        (window/swap-buffers window)

        (println "exiting")
        (catch Exception e
          (println "exception")
          (window/close window)
          (throw e))))))


(defn start []
  (start-view))

(run-tests)
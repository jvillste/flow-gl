(ns examples.opengl
  (:require [flow-gl.opengl :as opengl]
            [flow-gl.gui.event-queue :as event-queue]
            (flow-gl.opengl.jogl [window :as window]
                                 [triangle-list :as triangle-list]
                                 [image :as image]
                                 [texture :as texture])
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]))

  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]))

(defn start []
  (let [width 300
        height 300
        window (window/create width height (event-queue/create))
        gl (window/start-rendering window)]

    (try
      (opengl/initialize gl)
      (opengl/resize gl width height)

      (let [triangle-list (triangle-list/create-for-coordinates gl
                                                                :triangles
                                                                [0 0 width 0 (/ width 2) height]
                                                                (apply concat (repeat 3 [0 1 0 1])))

            pumpkin (image/create 0 0 (texture/create-for-buffered-image (buffered-image/create-from-file "pumpkin.png") gl)
                                  gl)

            hello (image/create 0 0 (texture/create-for-text "Hello World!"
                                                             [0 0 1 1]
                                                             (font/create "LiberationSans-Regular.ttf" 20)
                                                             gl)
                                gl)]

        (triangle-list/render gl triangle-list)
        (image/render pumpkin gl)
        (image/render hello gl)

        (triangle-list/delete gl triangle-list)
        ;(image/delete pumpkin gl)
        )

      (catch Exception e
        (let [string-writer (StringWriter.)]
          (.printStackTrace e (PrintWriter. string-writer))
          (println (.toString string-writer)))
        (window/close window)
        (throw e))
      (finally (try (opengl/dispose gl)
                    (catch Exception e))
               (window/end-rendering window)))))

(comment (start))

(ns examples.opengl
  (:require [flow-gl.opengl :as opengl]
            [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.opengl.jogl.window :as window]
            [flow-gl.opengl.jogl.triangle-list :as triangle-list])

  (:import [javax.media.opengl GL2]
           [java.io PrintWriter StringWriter]))

(let [width 200
      height 200
      window (window/create width height (event-queue/create))
      gl (window/start-rendering window)]

  (try

    (opengl/initialize gl)
    (opengl/resize gl width height)

    (let [triangle-list (triangle-list/create-for-coordinates gl
                                                              :triangles
                                                              [0 0 width 0 (/ width 2) height]
                                                              (apply concat (repeat 3 [0 1 0 1])))]
      (triangle-list/render gl triangle-list))


    #_(let [rectangle (drawable/->Rectangle 200 200 [0 1 0 1])
          commands (drawable/drawing-commands rectangle)
          runner (command/create-runner (first commands) gl)]
      (println "running")
      (command/run runner gl))

    (catch Exception e
      (let [string-writer (StringWriter.)]
        (.printStackTrace e (PrintWriter. string-writer))
        (println (.toString string-writer)))
      (window/close window)
      (throw e))
    (finally (try (opengl/dispose gl)
                  (catch Exception e))
             (window/end-rendering window))))

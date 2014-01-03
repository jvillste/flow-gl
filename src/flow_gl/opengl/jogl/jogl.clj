(ns flow-gl.opengl.jogl.jogl
  (:require [flow-gl.opengl :as opengl]
            [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.opengl.jogl.window :as window])

  (:import [javax.media.opengl GL2]))


(let [width 200
      height 200
      window (window/create width height (event-queue/create))
      gl (window/start-rendering window)]

  (try

    (doto gl
      (opengl/initialize)
      (opengl/resize width height)

      ;; DISPLAY
      ;;(.glClear GL/GL_COLOR_BUFFER_BIT)
      (.glColor3f 1.0 0.0 0.0)

      (.glBegin GL2/GL_TRIANGLES)
      (.glVertex2f 0 0)
      (.glVertex2f width 0)
      (.glVertex2f (/ width 2)  height)
      (.glEnd))
    (catch Exception e
      (window/close window)
      (throw e))
    (finally (opengl/dispose gl)
             (window/end-rendering window))))

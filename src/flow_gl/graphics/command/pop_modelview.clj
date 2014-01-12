(ns flow-gl.graphics.command.pop-modelview
  (:require [flow-gl.graphics.command :as command])
  (:import [javax.media.opengl GL2]))

(defrecord PopModelview [])

(extend PopModelview
  command/Command
  {:create-runner (fn [this gl] this)}
  command/CommandRunner
  {:delete (fn [this gl] this)
   :run (fn [_ gl]
          (.glMatrixMode gl GL2/GL_MODELVIEW)
          (.glPopMatrix gl))})

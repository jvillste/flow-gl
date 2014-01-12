(ns flow-gl.graphics.command.push-modelview
  (:require [flow-gl.graphics.command :as command])
  (:import [javax.media.opengl GL2]))

(defrecord PushModelview [])

(extend PushModelview
  command/Command
  {:create-runner (fn [this gl] this)}
  command/CommandRunner
  {:delete (fn [this gl] this)
   :run (fn [_ gl]
          (.glMatrixMode gl GL2/GL_MODELVIEW)
          (.glPushMatrix gl))})

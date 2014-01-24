(ns flow-gl.graphics.command.translate
  (:require [flow-gl.graphics.command :as command]
            (flow-gl.graphics.command 
                                   [push-modelview :as push-modelview]
                                   [pop-modelview :as pop-modelview]))
  (:import [javax.media.opengl GL2]))

(defrecord Translate [x y])

(defn combine [translate1 translate2]
  (->Translate (+ (:x translate1)
                  (:x translate2))
               (+ (:y translate1)
                  (:y translate2))))

(extend Translate
  command/Command
  {:create-runner (fn [this gl] this)}
  command/CombinableCommand
  {:combine combine}
  command/CommandRunner
  {:delete (fn [translate gl] translate)
   :run (fn [{:keys [x y]} gl]
          (.glMatrixMode gl GL2/GL_MODELVIEW)
          (.glTranslatef gl x y 0))})

(defn create [x y]
  (->Translate x y))

(defn translate [x y & commands]
  (concat [(push-modelview/->PushModelview)
           (create x y)]
          commands
          [(pop-modelview/->PopModelview)]))

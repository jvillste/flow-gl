(ns flow-gl.graphics.command.scale
  (:require [flow-gl.graphics.command :as command])
  (:import [javax.media.opengl GL2]))

(defrecord Scale [ratio])

(defn combine [scale1 scale2]
  (->Scale (* (:ratio scale1)
              (:ratio scale2))))

(extend Scale
  command/Command
  {:create-runner (fn [this gl] this)}
  command/CombinableCommand
  {:combine combine}
  command/CommandRunner
  {:delete (fn [this gl] this)
   :run (fn [{:keys [ratio]} gl]
          (.glMatrixMode gl GL2/GL_MODELVIEW)
          (.glScalef gl ratio ratio 1))})

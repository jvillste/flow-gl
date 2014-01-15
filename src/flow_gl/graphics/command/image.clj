(ns flow-gl.graphics.command.image
  (:require (flow-gl.graphics [command :as command]
                              [buffered-image :as buffered-image])
            (flow-gl.opengl.jogl [texture :as texture]
                                 [textured-quad :as textured-quad])))

(defrecord Image [file-name])
(defrecord ImageRunner [textured-quad])

(defn create [file-name] (->Image file-name))

(defn create-image-runner-for-buffered-image [buffered-image gl]
  (->ImageRunner (-> buffered-image
                     (texture/create-for-buffered-image gl)
                     (textured-quad/create gl))))

(defn create-image-runner [image gl]
  (create-image-runner-for-buffered-image (buffered-image/create-from-file (:file-name image))
                                          gl))

(extend Image
  command/Command
  {:create-runner create-image-runner})

(extend ImageRunner
  command/CommandRunner
  {:delete (fn [image-runner gl] (textured-quad/delete (:textured-quad image-runner)
                                                       gl))
   :run (fn [image-runner gl] (textured-quad/render (:textured-quad image-runner)
                                                    gl))})

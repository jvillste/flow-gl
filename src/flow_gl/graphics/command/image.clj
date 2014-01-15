(ns flow-gl.graphics.command.image
  (:require (flow-gl.graphics [image-list :as image-list]
                              [command :as command]
                              [buffered-image :as buffered-image])
            (flow-gl.opengl.jogl [texture :as texture]
                                 [image :as image])))

(defrecord Image [x y file-name])

(defrecord ImageList [images])

(defrecord ImageListRunner [image-list])

(defn load-texture [file-name gl]
  (-> (buffered-image/create-from-file file-name)
      (texture/create-for-buffered-image gl)
      (texture/load gl)))

(defn create-image-list [images gl]
  (reduce (fn [image-list [index image]]
            (image-list/add-image-for-texture image-list
                                              index
                                              (:x image)
                                              (:y image)
                                              (load-texture (:file-name image)
                                                            gl)
                                              gl))
          (image-list/create)
          (map-indexed vector images)))

(defn combine-image-lists [image-list1 image-list2]
  (->ImageList (concat (:images image-list1)
                       (:images image-list2))))

(defn create-image-list-runner [image-list gl]
  (->ImageListRunner (create-image-list (:images image-list)
                                        gl)))


(extend ImageList
  command/Command
  {:create-runner create-image-list-runner}
  command/CombinableCommand
  {:combine combine-image-lists})

(extend ImageListRunner
  command/CommandRunner
  {:delete (fn [image-list-runner gl] (image-list/delete (:image-list image-list-runner)
                                                         gl))
   :run (fn [image-list-runner gl] (image-list/draw (:image-list image-list-runner)
                                                 gl))})

(defn create [x y file-name] (->ImageList [(->Image x y file-name)]))

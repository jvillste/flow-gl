(ns flow-gl.graphics.image-list
  (:refer-clojure :exclude (load))
  (:require [flow-gl.opengl.jogl.texture :as texture]
            [flow-gl.opengl.jogl.image :as image]
            [flow-gl.data.zipper-list :as zipper-list])
  (:import [java.awt Color AlphaComposite]))

(defrecord ImageList [images
                      id-list])

(defn create []
  (ImageList. {}
              (zipper-list/create)))

(defn add-image-for-texture [image-list id x y texture gl]
  (assoc image-list
    :images (assoc (:images image-list)
              id (image/create x
                               y
                               texture
                               gl))
    :id-list (zipper-list/add (:id-list image-list)
                              id)))

(defn add-image [image-list id x y width height gl]
  (add-image-for-texture image-list
                         id
                         x
                         y
                         (-> (texture/create width height gl)
                             (texture/load gl))))

(defn update-image [image-list id updater]
  (assoc image-list
    :images (assoc (:images image-list)
              id (updater ((:images image-list) id)))))

(defn move-image [image-list id x y gl]
  (update-image image-list
                id
                #(image/move % x y gl)))

(defn resize-image [image-list id width height gl]
  (update-image image-list
                id
                #(image/set-texture % (texture/create width height) gl)))

(defn get-graphics [image-list id]
  (texture/get-graphics (:texture ((:images image-list) id))))

(defn draw-on-image [image-list id drawer gl]
  (drawer (get-graphics image-list id))
  (texture/load (:texture ((:images image-list) id))
                gl)
  image-list)

(defn clear-image [image-list id gl]
  (doto (get-graphics image-list id)
    (.setComposite (AlphaComposite/getInstance AlphaComposite/CLEAR (float 0)))
    (.fillRect 0
               0
               (image/width ((:images image-list) id))
               (image/height ((:images image-list) id))))
  (texture/load (:texture ((:images image-list) id))
                gl))

(defn draw [image-list gl]
  (doseq [image-id (zipper-list/items (:id-list image-list))]
    (image/render ((:images image-list) image-id)
                  gl))
  image-list)

(defn delete [image-list gl]
  (doseq [image (vals (:images image-list))]
    (image/delete image gl)))

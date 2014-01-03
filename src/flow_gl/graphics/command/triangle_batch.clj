(ns flow-gl.graphics.command.triangle-batch
  (:require [flow-gl.opengl.jogl.triangle-list :as triangle-list]
            [flow-gl.graphics.command :as command]))

(defrecord TriangleBatch [coordinates colors])

(defrecord TriangleBatchRunner [triangle-list])

(defn create [coordinates colors]
  (->TriangleBatch coordinates colors))

(defn concatenate [triangle-batch1 triangle-batch2]
  (->TriangleBatch (concat (:coordinates triangle-batch1)
                           (:coordinates triangle-batch2))
                   (concat (:colors triangle-batch1)
                           (:colors triangle-batch2))))

(defn number-of-triangles [triangle-batch]
  (/ (count (:coordinates triangle-batch))
     6))

(defn create-triangle-list [gl triangle-batch]
  (triangle-list/create-for-coordinates gl
                                        :triangles
                                        (:coordinates triangle-batch)
                                        (:colors triangle-batch)))

(defn update-triangle-list-from-triangle-batch [gl triangle-list triangle-batch]
  (triangle-list/update gl triangle-list (:coordinates triangle-batch) (:colors triangle-batch)))

(defn create-triangle-batch-runner [triangle-batch gl]
  (->  (create-triangle-list gl triangle-batch)
       (->TriangleBatchRunner)))

(extend TriangleBatch
  command/Command
  {:create-runner create-triangle-batch-runner}
  command/CombinableCommand
  {:combine concatenate})

(extend TriangleBatchRunner
  command/CommandRunner
  {:delete (fn [triangle-batch-runner gl]  (triangle-list/delete gl (:triangle-list triangle-batch-runner)))
   :run (fn [triangle-batch-runner gl] (triangle-list/render gl (:triangle-list triangle-batch-runner)))})

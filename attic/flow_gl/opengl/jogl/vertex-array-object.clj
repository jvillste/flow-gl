(ns flow-gl.opengl.jogl.vertex-array-object
  (:refer-clojure :exclude [read]))

(defn create [gl]
  (let [result-buffer (int-array 1)]
    (.glGenVertexArrays gl 1 result-buffer 0)
    (aget result-buffer 0)))

(defn delete [gl id]
  (.glDeleteVertexAarrays gl 1 (int-array [id]) 0))

(defn bind [gl id]
  (.glBindVertexArray gl id))


(ns flow-gl.opengl.jogl.vertex-array-object
  (:refer-clojure :exclude [read])
  (:require [flow-gl.graphics.native-buffer :as native-buffer])
  (:import [com.jogamp.opengl GL2]))

(defn create [gl]
  (let [result-buffer (int-array 1)]
    (.glGenVertexArrays gl 1 result-buffer 0)
    (aget result-buffer 0)))

(defn delete [gl id]
  (.glDeleteVertexArrays gl 1 (int-array [id]) 0))

(defn bind [gl id]
  (.glBindVertexArray gl id))


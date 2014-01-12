(ns flow-gl.opengl.jogl.buffer
  (:require [flow-gl.graphics.native-buffer :as native-buffer])
  (:import [javax.media.opengl GL2]))

(defn create-gl-buffer [gl]
  (let [result-buffer (int-array 1)]
    (.glGenBuffers gl 1 result-buffer 0)
    (aget result-buffer 0)))

(defn bind-buffer [gl id]
  (.glBindBuffer gl GL2/GL_ARRAY_BUFFER id))

(defn delete [gl id]
  (.glDeleteBuffers gl 1 (int-array [id]) 0))

(defn bind-element-buffer [gl id]
  (.glBindBuffer gl GL2/GL_ELEMENT_ARRAY_BUFFER id))

(defn load-buffer [gl id type values]
  (let [native-buffer (native-buffer/native-buffer-with-values type values)]
    (bind-buffer gl id)
    (.glBufferData gl
                   GL2/GL_ARRAY_BUFFER
                   (* 4 (count values))
                   native-buffer
                   GL2/GL_STATIC_DRAW)))

(defn load-element-buffer [gl id values]
  (let [native-buffer (native-buffer/native-buffer-with-values :int values)]
    (bind-element-buffer gl id)
    (.glBufferData gl
                   GL2/GL_ELEMENT_ARRAY_BUFFER
                   native-buffer
                   GL2/GL_STATIC_DRAW)))

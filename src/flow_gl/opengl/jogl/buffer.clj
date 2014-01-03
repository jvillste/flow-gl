(ns flow-gl.opengl.jogl.buffer
  (:require [flow-gl.graphics.native-buffer :as native-buffer])
  (:import [javax.media.opengl GL2]))

(defn create-gl-buffer [gl] (.glGenBuffers gl))

(defn bind-buffer [gl id]
  (.glBindBuffer GL2/GL_ARRAY_BUFFER id))

(defn delete [gl id]
  (.glDeleteBuffers gl id))

(defn bind-element-buffer [gl id]
  (.glBindBuffer GL2/GL_ELEMENT_ARRAY_BUFFER id))


(defn load-buffer [gl id type values]
  (let [native-buffer (native-buffer/native-buffer-with-values type values)]
    (bind-buffer id)
    (.glBufferData gl
                   GL2/GL_ARRAY_BUFFER
                   native-buffer
                   GL2/GL_STATIC_DRAW)))

(defn load-element-buffer [gl id values]
  (let [native-buffer (native-buffer/native-buffer-with-values :int values)]
    (bind-element-buffer id)
    (.glBufferData gl
                   GL2/GL_ELEMENT_ARRAY_BUFFER
                   native-buffer
                   GL2/GL_STATIC_DRAW)))

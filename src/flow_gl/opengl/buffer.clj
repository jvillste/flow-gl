(ns flow-gl.opengl.buffer
  (:require [flow-gl.graphics.native-buffer :as native-buffer])
  (:import [org.lwjgl.opengl ARBVertexBufferObject]
           [org.lwjgl BufferUtils]
           [java.nio FloatBuffer IntBuffer]))

(defn create-gl-buffer [] (ARBVertexBufferObject/glGenBuffersARB))

(defn bind-buffer [id]
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB id))

(defn delete [id]
  (ARBVertexBufferObject/glDeleteBuffersARB id))

(defn bind-element-buffer [id]
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB id))


(defn load-buffer [id type values]
  (let [native-buffer (native-buffer/native-buffer-with-values type values)]
    (bind-buffer id)
    (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                           native-buffer
                                           ARBVertexBufferObject/GL_STATIC_DRAW_ARB)))

(defn load-element-buffer [id values]
  (let [native-buffer (native-buffer/native-buffer-with-values :int values)]
    (bind-element-buffer id)
    (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB
                                           native-buffer
                                           ARBVertexBufferObject/GL_STATIC_DRAW_ARB)))

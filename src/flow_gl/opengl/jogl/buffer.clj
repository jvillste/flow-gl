(ns flow-gl.opengl.jogl.buffer
  (:require [flow-gl.graphics.native-buffer :as native-buffer])
  (:import [javax.media.opengl GL2]))

(defn create-gl-buffer [gl]
  (let [result-buffer (int-array 1)]
    (.glGenBuffers gl 1 result-buffer 0)
    (aget result-buffer 0)))

(defn delete [gl id]
  (.glDeleteBuffers gl 1 (int-array [id]) 0))

(defn bind-element-buffer [gl id]
  (.glBindBuffer gl GL2/GL_ELEMENT_ARRAY_BUFFER id))

(defn type-size [type]
  (case type
    :byte 1
    :int 4
    :float 4
    :short 2))

(defn copy [gl source target number-of-bytes]
  (.glBindBuffer gl GL2/GL_COPY_READ_BUFFER source)
  (.glBindBuffer gl GL2/GL_COPY_WRITE_BUFFER target)
  (.glCopyBufferSubData gl
                        GL2/GL_COPY_READ_BUFFER
                        GL2/GL_COPY_WRITE_BUFFER
                        0
                        0
                        number-of-bytes))

(defn allocate-buffer [gl id type target usage size]
  (.glBufferData gl
                 target
                 (* (type-size type)
                    size)
                 nil
                 usage))

(defn load-buffer [gl id type target usage values]
  (let [native-buffer (native-buffer/native-buffer-with-values type values)]
    (.glBindBuffer gl target id)
    (.glBufferData gl
                   target
                   (* (type-size type)
                      (count values))
                   native-buffer
                   usage)))

(defn load-vertex-array-buffer [gl id type values]
  (load-buffer gl id type GL2/GL_ARRAY_BUFFER GL2/GL_STATIC_DRAW values))

(defn load-texture-buffer [gl id type values]
  (load-buffer gl id type GL2/GL_TEXTURE_BUFFER GL2/GL_STATIC_DRAW values))

(defn load-element-buffer [gl id values]
  (load-buffer gl id :int GL2/GL_ELEMENT_ARRAY_BUFFER GL2/GL_STATIC_DRAW values))

(defn update [gl id type offset values]
  (let [native-buffer (native-buffer/native-buffer-with-values type values)]
    (.glBindBuffer gl GL2/GL_COPY_WRITE_BUFFER id)
    (.glBufferSubData gl
                      GL2/GL_COPY_WRITE_BUFFER
                      (* (type-size type)
                         offset)
                      (* (type-size type)
                         (count values))
                      native-buffer)))

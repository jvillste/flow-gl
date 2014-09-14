(ns flow-gl.opengl.jogl.buffer
  (:refer-clojure :exclude [read])
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

(defn copy [gl source target type read-offset write-offset size]
  (.glBindBuffer gl GL2/GL_COPY_READ_BUFFER source)
  (.glBindBuffer gl GL2/GL_COPY_WRITE_BUFFER target)
  (.glCopyBufferSubData gl
                        GL2/GL_COPY_READ_BUFFER
                        GL2/GL_COPY_WRITE_BUFFER
                        (* read-offset (type-size type))
                        (* write-offset (type-size type))
                        (* size (type-size type))))

(defn allocate-buffer [gl id type target usage size]
  (.glBindBuffer gl target id)
  (.glBufferData gl
                 target
                 (* (type-size type)
                    size)
                 nil
                 usage))

(defn load-buffer-from-native-buffer [gl id type target usage native-buffer]
  (.glBindBuffer gl target id)
  (.glBufferData gl
                 target
                 (* (type-size type)
                    (.limit native-buffer))
                 native-buffer
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

(defn update-from-native-buffer [gl id type offset count native-buffer]
  (.glBindBuffer gl GL2/GL_COPY_WRITE_BUFFER id)
  (.glBufferSubData gl
                    GL2/GL_COPY_WRITE_BUFFER
                    (* (type-size type)
                       offset)
                    (* (type-size type)
                       count)
                    native-buffer))

(defn update [gl id type offset values]
  (update-from-native-buffer gl id type offset (count values) (native-buffer/native-buffer-with-values type values)))

(defn map-for-write [gl id type offset length]
  (.glBindBuffer gl GL2/GL_COPY_WRITE_BUFFER id)
  (-> (.glMapBufferRange gl
                         GL2/GL_COPY_WRITE_BUFFER
                         (* (type-size type)
                            offset)
                         (* (type-size type)
                            length)
                         GL2/GL_MAP_WRITE_BIT)
      (as-> byte-buffer
            (case type
              :float (.asFloatBuffer byte-buffer)
              :int (.asIntBuffer byte-buffer)
              :short (.asShortBuffer byte-buffer)))))

(defn unmap-for-write [gl]
  (.glUnmapBuffer gl GL2/GL_COPY_WRITE_BUFFER))

(defn map-for-read [gl id type offset length]
  (.glBindBuffer gl GL2/GL_COPY_READ_BUFFER id)
  (-> (.glMapBufferRange gl
                         GL2/GL_COPY_READ_BUFFER
                         (* (type-size type)
                            offset)
                         (* (type-size type)
                            length)
                         GL2/GL_MAP_READ_BIT)
      (as-> byte-buffer
            (case type
              :float (.asFloatBuffer byte-buffer)
              :int (.asIntBuffer byte-buffer)
              :short (.asShortBuffer byte-buffer)))))

(defn unmap-for-read [gl]
  (.glUnmapBuffer gl GL2/GL_COPY_READ_BUFFER))

(defn read [gl id type  offset length]
  (let [array (case type
                :float (float-array length)
                :int (int-array length)
                :short (short-array length))]
    (.glBindBuffer gl GL2/GL_COPY_READ_BUFFER id)
    (-> (.glMapBufferRange gl
                           GL2/GL_COPY_READ_BUFFER
                           (* (type-size type)
                              offset)
                           (* (type-size type)
                              length)
                           GL2/GL_MAP_READ_BIT)
        (as-> byte-buffer
              (case type
                :float (.asFloatBuffer byte-buffer)
                :int (.asIntBuffer byte-buffer)
                :short (.asShortBuffer byte-buffer)))
        (.get array))
    (.glUnmapBuffer gl GL2/GL_COPY_READ_BUFFER)
    array))

(ns flow-gl.opengl.jogl.frame-buffer
  (:import [javax.media.opengl GL2]))

(defn create [gl]
  (let [result-buffer (int-array 1)]
    (.glGenFrameBuffers gl 1 result-buffer 0)
    (aget result-buffer 0)))

(defn bind [gl id]
  (.glBindFramebuffer gl GL2/GL_FRAMEBUFFER id))

(defn delete [gl id]
  (.glDeleteFrameBuffers gl 1 (int-array [id]) 0))

(defn bind-texture [gl texture-id]
  (.glFramebufferTexture2D gl
                           GL2/GL_FRAMEBUFFER
                           GL2/GL_COLOR_ATTACHMENT0
                           GL2/GL_TEXTURE_2D
                           texture-id
                           0))

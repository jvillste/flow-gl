(ns flow-gl.opengl.jogl.frame-buffer
  (:require (flow-gl.opengl.jogl [get :as get]))
  (:import [com.jogamp.opengl GL2]))

(defn create [gl]
  (let [result-buffer (int-array 1)]
    (.glGenFramebuffers gl (int 1) result-buffer (int 0))
    (aget result-buffer 0)))

(defn bind [id gl]
  (.glBindFramebuffer gl GL2/GL_FRAMEBUFFER id))

(defn bind-read [id gl]
  (.glBindFramebuffer gl GL2/GL_READ_FRAMEBUFFER id))

(defn bind-draw [id gl]
  (.glBindFramebuffer gl GL2/GL_DRAW_FRAMEBUFFER id))

(defn delete [id gl]
  (.glDeleteFramebuffers gl (int 1) (int-array [id]) (int 0)))


(defn get-read [gl]
  (get/integer gl GL2/GL_READ_FRAMEBUFFER_BINDING))

(defn get-draw [gl]
  (get/integer gl GL2/GL_DRAW_FRAMEBUFFER_BINDING))

(defn bind-texture [texture-id gl]
  (.glFramebufferTexture2D gl
                           GL2/GL_FRAMEBUFFER
                           GL2/GL_COLOR_ATTACHMENT0
                           GL2/GL_TEXTURE_2D
                           texture-id
                           0))

(defn bind-stencil [render-buffer-id gl]
  (.glFramebufferRenderbuffer gl
                              GL2/GL_FRAMEBUFFER
                              GL2/GL_STENCIL_ATTACHMENT
                              GL2/GL_RENDERBUFFER
                              render-buffer-id))

(defn blit-framebuffer [gl source target width height]
  (doto gl
    (.glBindFramebuffer GL2/GL_READ_FRAMEBUFFER source)
    (.glBindFramebuffer GL2/GL_DRAW_FRAMEBUFFER target)
    (.glBlitFramebuffer 0 0 width height 0 0 width height GL2/GL_COLOR_BUFFER_BIT GL2/GL_LINEAR)))

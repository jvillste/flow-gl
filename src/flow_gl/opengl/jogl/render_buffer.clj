(ns flow-gl.opengl.jogl.render-buffer
  (:refer-clojure :exclude (load))
  (:import [javax.media.opengl GL2]))

(defn create [gl]
  (let [result (int-array 1)]
    (.glGenRenderbuffers gl 1 result 0)
    (first result)))

(defn delete [render-buffer gl]
  (.glDeleteRenderbuffers gl 1 (int-array [render-buffer]) 0))

(defn bind [render-buffer gl]
  (.glBindRenderbuffer gl GL2/GL_RENDERBUFFER render-buffer))

(defn create-stencil-buffer [width height gl]
  (let [render-buffer (create gl)]
    (bind render-buffer gl)
    (.glRenderbufferStorage gl GL2/GL_RENDERBUFFER GL2/GL_DEPTH_STENCIL #_GL_STENCIL_INDEX8 width height)
    (bind 0 gl)
    render-buffer))










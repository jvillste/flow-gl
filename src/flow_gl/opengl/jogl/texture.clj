(ns flow-gl.opengl.jogl.texture
  (:refer-clojure :exclude (load))
  (:import [javax.media.opengl GL2]
           [com.jogamp.opengl.util.texture TextureIO]
           [com.jogamp.opengl.util.texture.awt AWTTextureIO])
  (:require [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.font :as font]))

(defn- texture-dimension [value] value)

(defn create-gl-texture [gl]
    (let [result (int-array 1)]
      (.glGenTextures gl 1 result 0)
      (first result)))

(defn delete-gl-texture [texture gl]
  (.glDeleteTextures gl 1 (int-array [texture]) 0))

(defrecord Texture [texture width height buffered-image])

(defn delete [texture gl]
  ;;(.glDeleteTextures gl 1 (int-array [(:id texture)]) 0)
  (.destroy (:texture texture) gl))

(defn bind [texture gl]
  ;; (.glBindTexture gl GL2/GL_TEXTURE_2D (:id texture))
  (.bind (:texture texture) gl))


(defn get-graphics [texture]
  (buffered-image/get-graphics (:buffered-image texture)))

(defn texture-x-to-texel-x [texture texture-x]
  (* texture-x
     (:width texture)))

(defn texture-y-to-texel-y [texture texture-y]
  (* texture-y
     (:height texture)))

(defn create-jogl-texture [buffered-image gl]
  ;;(println "creating texture")
  (let [texture (AWTTextureIO/newTexture (.getGLProfile gl) buffered-image false)]
    ;;(println (.getWidth texture) " X " (.getHeight texture) " = " (.getEstimatedMemorySize texture))
    texture))

(defn create-for-buffered-image [buffered-image gl]
  (Texture. (create-jogl-texture buffered-image gl)
            (.getWidth buffered-image)
            (.getHeight buffered-image)
            buffered-image))

(defn create-for-text [text color font gl]
  (let [buffered-image (buffered-image/create (max 1
                                                   (font/width font text))
                                              (max 1
                                                   (font/height font)))]

    (text/draw (buffered-image/get-graphics buffered-image)
               color
               font
               text)

    (create-for-buffered-image buffered-image gl)))

(defn create
  ([minimum-width minimum-height gl]
     (create-for-buffered-image (buffered-image/create (texture-dimension minimum-width)
                                                       (texture-dimension minimum-height))
                                gl))

  ([gl]
     (create 128 128 gl)))


(defn load [texture gl]
  (delete texture gl)
  (create-for-buffered-image (:buffered-image texture) gl))

(defn create-child [texture x y width height gl]
  (create-for-buffered-image (buffered-image/create-child (:buffered-image texture) x y width height)
                             gl))

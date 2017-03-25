(ns flow-gl.opengl.jogl.texture
  (:refer-clojure :exclude (load))
  (:import [javax.media.opengl GL2]
           [com.jogamp.opengl.util.texture TextureIO]
           [com.jogamp.opengl.util.texture.awt AWTTextureIO])
  (:require [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.font :as font]
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [native-buffer :as native-buffer])))

(defn create-texture-object [gl]
  (let [result (int-array 1)]
    (.glGenTextures gl 1 result 0)
    (first result)))

(defn create [gl]
  (let [texture (create-texture-object gl)]
    (.glBindTexture gl GL2/GL_TEXTURE_2D texture)

    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_S GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_WRAP_T GL2/GL_CLAMP_TO_EDGE)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MAG_FILTER GL2/GL_NEAREST)
    (.glTexParameteri gl GL2/GL_TEXTURE_2D GL2/GL_TEXTURE_MIN_FILTER GL2/GL_NEAREST)

    texture))

(defn delete [gl texture]
  (assert (number? texture))
  (.glDeleteTextures gl 1 (int-array [texture]) 0))

(defn load [gl texture width height data]
  (.glBindTexture gl GL2/GL_TEXTURE_2D texture)
  (.glTexImage2D gl GL2/GL_TEXTURE_2D 0 GL2/GL_RGBA8 width height 0 GL2/GL_BGRA GL2/GL_UNSIGNED_BYTE data))

(defn load-1d-int [gl texture size data]
  (.glBindTexture gl GL2/GL_TEXTURE_1D texture)
  (.glTexImage1D gl GL2/GL_TEXTURE_1D 0 GL2/GL_R16I size 0 GL2/GL_RED_INTEGER GL2/GL_INT (native-buffer/create-native-buffer-with-values data)))

(defn load-from-buffered-image [gl texture image]
  (load gl texture  (.getWidth image) (.getHeight image)
        (native-buffer/create-native-buffer-with-values (-> image (.getRaster) (.getDataBuffer) (.getData)))))

(defn copy-to-buffer [gl texture buffer offset]
  (assert (instance? Long offset))

  (.glBindTexture gl GL2/GL_TEXTURE_2D texture)
  (.glBindBuffer gl GL2/GL_PIXEL_PACK_BUFFER buffer)
  (.glGetTexImage gl GL2/GL_TEXTURE_2D 0 GL2/GL_BGRA GL2/GL_UNSIGNED_BYTE offset))

(defn create-for-buffered-image [buffered-image gl]
  (let [texture (create gl)]
    (load-from-buffered-image gl texture buffered-image)
    texture))

(defn create-for-file [file-name gl]
  (let [image (buffered-image/create-from-file file-name)
        texture (create gl)]
    (load-from-buffered-image gl texture image)
    texture))

(defn bind [gl texture]
  (assert number? texture)
  (.glBindTexture gl GL2/GL_TEXTURE_2D texture))








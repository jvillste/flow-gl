(ns flow-gl.graphics.buffered-image
  (:require [clojure.java.io :as io])
  (:import (java.awt Color)
           (java.awt.color ColorSpace)
           (java.awt.image BufferedImage ComponentColorModel DataBuffer)
           (java.io File)
           (java.nio ByteBuffer ByteOrder)
           (java.util Hashtable)
           (javax.imageio ImageIO)
           [com.sun.imageio.plugins.gif GIFImageReader GIFImageReaderSpi]
           [javax.imageio ImageIO]))

(defn create-byte-buffer [buffered-image]
  (let [bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))
        byte-buffer (ByteBuffer/allocateDirect (alength bytes))]
    (.order byte-buffer (ByteOrder/nativeOrder))
    (.put byte-buffer bytes 0 (alength bytes))
    (.flip byte-buffer)))


(defn- create-from-raster [raster]
  (let [component-color-model (ComponentColorModel. (ColorSpace/getInstance ColorSpace/CS_sRGB)
                                                    (int-array [8 8 8 8])
                                                    true
                                                    false
                                                    ComponentColorModel/TRANSLUCENT
                                                    DataBuffer/TYPE_BYTE)]
    (BufferedImage. component-color-model
                    raster
                    false
                    (Hashtable.))))

(defn get-color [buffered-image x y]
  (let [color (Color. (.getRGB buffered-image x y) true)]
    [(.getRed color) (.getGreen color) (.getBlue color) (.getAlpha color)]))

(defn create [width height]
  (assert (and (< width 10000)
               (< height 10000))
          (str "Tried to create buffered image larger than 10.000 x 10.000. The requested size was " width " x " height))

  (BufferedImage. (max 1 width) (max 1 height) BufferedImage/TYPE_INT_ARGB)
  #_(create-from-raster (Raster/createInterleavedRaster DataBuffer/TYPE_BYTE
                                                        width
                                                        height
                                                        4
                                                        nil)))

(defn get-graphics [buffered-image]
  (.createGraphics buffered-image))

(defn clear [buffered-image r g b a]
  (let [graphics (get-graphics buffered-image)]
    (.setBackground graphics (Color. r g b a))
    (.clearRect graphics 0 0 (.getWidth buffered-image) (.getHeight buffered-image))))

(defn copy [input-buffered-image]
  (let [width (.getWidth input-buffered-image)
        height (.getHeight input-buffered-image)
        new-buffered-image (create width height)]
    (.drawImage (get-graphics new-buffered-image)
                input-buffered-image
                0 0
                nil)
    new-buffered-image))

(defn clip [input-buffered-image x y width height]
  (let [new-buffered-image (create width height)]
    (.drawImage (get-graphics new-buffered-image)
                input-buffered-image
                0 0
                width height
                x y
                (+ x width)
                (+ y width)
                nil)
    new-buffered-image))

(defn gif-frames [file]
  (vec (let [image-reader (GIFImageReader. (GIFImageReaderSpi.))]
         (.setInput image-reader (ImageIO/createImageInputStream (io/file file)))
         (for [index (range (.getNumImages image-reader true))]
           (copy (.read image-reader index))))))

(defn draw-image [graphics image width height]
  (.drawImage graphics
              image
              0
              0
              (or width 100)
              (or height 100)
              nil))

(defn create-from-file [url]
  (let [original-image (ImageIO/read (io/input-stream url))
        new-image (create (.getWidth original-image)
                          (.getHeight original-image))]
    (.drawImage (get-graphics new-image)
                original-image
                nil
                0
                0)
    new-image))

(defn resize [original-image width height]
  (let [new-image (create width height)]
    (.drawImage (get-graphics new-image)
                original-image
                0
                0
                width
                height
                nil)
    new-image))

(defn create-resized-from-file [file-name width height]
  (let [original-image (ImageIO/read (File. file-name))
        new-image (create width
                          height)]
    (.drawImage (get-graphics new-image)
                original-image
                0 0
                width height
                0 0
                (.getWidth original-image) (.getHeight original-image)
                nil)
    new-image))



(defn create-child [parent x y width height]
  (-> parent
      (.getRaster)
      (.createWritableChild x y width height 0 0 nil)
      (create-from-raster)))

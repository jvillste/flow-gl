(ns flow-gl.graphics.buffered-image
  (:import [java.awt.image BufferedImage Raster DataBuffer ComponentColorModel]
           [java.awt.color ColorSpace]
           [java.awt Color]
           [java.util Hashtable]
           [java.nio IntBuffer FloatBuffer ByteBuffer ByteOrder]
           [javax.imageio ImageIO]
           [java.io File]))

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

(defn create [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
  #_(create-from-raster (Raster/createInterleavedRaster DataBuffer/TYPE_BYTE
                                                      width
                                                      height
                                                      4
                                                      nil)))

(defn get-graphics [buffered-image]
  (.createGraphics buffered-image))

(defn clear [buffered-image]
  (let [graphics (get-graphics buffered-image)]
    (.setBackground graphics (Color. 255 255 255 0))
    (.clearRect graphics 0 0 (.getWidth buffered-image) (.getHeight buffered-image))))

(defn create-from-file [file-name]
  (let [original-image (ImageIO/read (File. file-name))
        new-image (create (.getWidth original-image)
                          (.getHeight original-image))]
    (.drawImage (get-graphics new-image)
                original-image
                nil
                0
                0)
    new-image))



(defn create-child [parent x y width height]
  (-> parent
      (.getRaster)
      (.createWritableChild x y width height 0 0 nil)
      (create-from-raster)))

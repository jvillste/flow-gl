(ns flow-gl.gui.drawable
  (:require  (flow-gl.graphics [font :as font]
                               [vector :as vector]
                               [text :as graphics-text]
                               [buffered-image :as buffered-image])
             (flow-gl.gui [layoutable :as layoutable]))
  (:import [java.awt.geom Rectangle2D$Float RoundRectangle2D$Float GeneralPath]
           [java.awt Color RenderingHints BasicStroke]
           [nanovg NanoVG]))

(defprotocol Java2DDrawable
  (draw [this graphics]))

(defprotocol NanoVGDrawable
  (draw-nanovg [this nanovg]))

(defprotocol GLDrawable
  (draw-gl [this gl]))

;; DRAWABLES

(defrecord Quad [texture width height]

  GLDrawable
  (draw-gl [this gl]
    )

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    {:width width
     :height height})

  Object
  (toString [this] (layoutable/describe-layoutable this)))

(defrecord Text [contents font color]

  Java2DDrawable
  (draw [this graphics]
    (graphics-text/draw graphics color font contents))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    {:width (font/width font contents)
     :height (font/height font)})

  Object
  (toString [this] (layoutable/describe-layoutable this)))

(defrecord Image [buffered-image]
  Java2DDrawable
  (draw [this graphics]
    (doto graphics
      (.drawImage buffered-image (int 0) (int 0) nil)))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    {:width (.getWidth buffered-image)
     :height (.getHeight buffered-image)})
  (preferred-width [this] 100)
  (preferred-height [this] 100)

  Object
  (toString [this] (layoutable/describe-layoutable this "Image" :file-name)))

(defrecord Empty [width height]
  layoutable/Layoutable
  (preferred-size [this available-width available-height] {:width width
                                                           :height height})
  (preferred-width [empty] width)
  (preferred-height [empty] height)

  Java2DDrawable
  (draw [this graphics])

  Object
  (toString [this] (layoutable/describe-layoutable this "Empty" :width :height)))

(defrecord Rectangle [width height color]
  layoutable/Layoutable
  (preferred-size [this available-width available-height] {:width width
                                                           :height height})

  #_Java2DDrawable
  #_(draw [this graphics]
    (println "drawing rectangle")
    (let [[r g b a] (map float color)]
      (doto graphics
        (.setColor (Color. r g b a))
        (.fill (Rectangle2D$Float. 0 0 width height)))))

  NanoVGDrawable
  (draw-nanovg [this nanovg]
    (let [[r g b a] color]
      (doto nanovg
        (NanoVG/fillColor (char r) (char g) (char b) (char a))
        (NanoVG/beginPath)
        (NanoVG/rect 0 0 width height)
        (NanoVG/fill))))

  Object
  (toString [this] (layoutable/describe-layoutable this)))






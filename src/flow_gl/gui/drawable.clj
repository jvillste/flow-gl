(ns flow-gl.gui.drawable
  (:require  (flow-gl.graphics [font :as font]
                               [text :as graphics-text]
                               [buffered-image :as buffered-image])
             (flow-gl.gui [layoutable :as layoutable])
             (flow-gl.opengl.jogl [quad :as quad]
                                  [opengl :as opengl])
             [flow-gl.opengl.math :as math])
  (:import [java.awt.geom Rectangle2D$Float RoundRectangle2D$Float GeneralPath]
           [java.awt Color RenderingHints BasicStroke]
           [nanovg NanoVG]))

(defprotocol Java2DDrawable
  (draw [this graphics]))

(defprotocol NanoVGDrawable
  (draw-nanovg [this nanovg]))

(defprotocol TriangleListDrawable
  (triangles [this]))

#_(defprotocol TextureDrawable
    (draw-texture [this texture-id]))


;; DRAWABLES

#_(defrecord Texture [texture-id width height]

    layoutable/Layoutable
    (preferred-size [this available-width available-height]
      {:width width
       :height height})

    Object
    (toString [this] (layoutable/describe-layoutable this)))


(defrecord Quad [textures uniforms fragment-shader-source x y width height]

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
  #_NanoVGDrawable
  #_(draw-nanovg [this nanovg]
      (let [[r g b a] color]
        (doto nanovg
          (NanoVG/fillColor (char r) (char g) (char b) (char a))
          (NanoVG/beginPath)
          (NanoVG/rect 0 0 width height)
          (NanoVG/fill))))

  TriangleListDrawable
  (triangles [this]
    [(math/quad (:x this) (:y this) width height)
     (apply concat (repeat 6 color))])

  Object
  (toString [this] (layoutable/describe-layoutable this)))











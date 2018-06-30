(ns flow-gl.gui.drawable
  (:require [flow-gl.graphics.font :as font]
            [flow-gl.graphics.text :as graphics-text]
            [flow-gl.gui.layoutable :as layoutable]
            [flow-gl.opengl.math :as math]))

(defprotocol Java2DDrawable
  (draw [this graphics]))

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
    {:width (or (:width this)
                (font/width font contents))
     :height (or (:height this)
                 (font/height font))})

  Object
  (toString [this] (layoutable/describe-layoutable this)))

(defrecord Image [buffered-image]
  Java2DDrawable
  (draw [this graphics]
    (doto graphics
      (.drawImage buffered-image (int 0) (int 0) nil)))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    {:width (or (:width this) (.getWidth buffered-image))
     :height (or (:width this) (.getHeight buffered-image))})

  Object
  (toString [this] (layoutable/describe-layoutable this "Image" :file-name)))

(defrecord Empty [width height]
  layoutable/Layoutable
  (preferred-size [this available-width available-height] {:width width
                                                           :height height})

  Java2DDrawable
  (draw [this graphics])

  Object
  (toString [this] (layoutable/describe-layoutable this "Empty" :width :height)))

(defrecord Rectangle [width height color]
  layoutable/Layoutable
  (preferred-size [this available-width available-height] {:width width
                                                           :height height})

  TriangleListDrawable
  (triangles [this]
    [(math/quad (:x this) (:y this) width height)
     (apply concat (repeat 6 (map (fn [value] (/ value 255)) color)))])

  Object
  (toString [this] (layoutable/describe-layoutable this)))











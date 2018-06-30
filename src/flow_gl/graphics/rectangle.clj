(ns flow-gl.graphics.rectangle
  (:require [flow-gl.graphics.buffered-image :as buffered-image])
  (:import (java.awt BasicStroke Color RenderingHints)
           (java.awt.geom RoundRectangle2D$Double)))

(defn fill [graphics color width height corner-arc-width corner-arc-height]
  (let [[r g b a] (map (fn [color] (float (/ color 255)))
                       color)]
    (doto graphics
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setColor (Color. r g b a))
      (.fill (RoundRectangle2D$Double. (double 0) (double 0) (double width) (double height) (double corner-arc-width) (double corner-arc-height))))))

(defn draw [graphics color line-width width height corner-arc-width corner-arc-height]
  (let [[r g b a] (map (fn [color] (float (/ color 255)))
                       color)
        half-line-width (/ line-width 2)]
    (doto graphics
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setColor (Color. r g b a))
      (.setStroke (BasicStroke. line-width))
      (.draw (RoundRectangle2D$Double. (double half-line-width) (double half-line-width) (double (- width line-width)) (double (- height line-width)) (double corner-arc-width) (double corner-arc-height))))))

(defn create-buffered-image
  "Deprecated, use buffered-image/create instead."
  [color width height corner-arc-width corner-arc-height]
  (assert (and (< width 10000)
               (< height 10000))
          (str "Tried to create buffered image larger than 10.000 x 10.000. The requested size was " width " x " height))

  (let [buffered-image (buffered-image/create (max 1
                                                   width)
                                              (max 1
                                                   height))]

    (fill (buffered-image/get-graphics buffered-image)
          color width height corner-arc-width corner-arc-height)

    buffered-image))

(defn contains [width height corner-arc-width corner-arc-height x y]
  (.contains (RoundRectangle2D$Double. (double 0) (double 0) (double width) (double height) (double corner-arc-width) (double corner-arc-height))
             (double x)
             (double y)))

(ns flow-gl.graphics.rectangle
  (:require (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [java.awt Color RenderingHints]
           [java.awt.geom RoundRectangle2D$Double]))


(defn create-buffered-image [color width height corner-arc-width corner-arc-height]
  (let [buffered-image (buffered-image/create (max 1
                                                   width)
                                              (max 1
                                                   height))]

    (let [[r g b a] (map (fn [color] (float (/ color 255)))
                         color)]
      (doto (buffered-image/get-graphics buffered-image)
        (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        (.setColor (Color. r g b a))
        (.fill (RoundRectangle2D$Double. (double 0) (double 0) (double width) (double height) (double corner-arc-width) (double corner-arc-height)))))

    buffered-image))

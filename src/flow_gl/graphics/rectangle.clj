(ns flow-gl.graphics.rectangle
  (:require (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [java.awt Color RenderingHints]
           [java.awt.geom RoundRectangle2D$Double]))

(defn draw [graphics color width height corner-arc-width corner-arc-height]
  (let [[r g b a] (map (fn [color] (float (/ color 255)))
                       color)]
    (doto graphics
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setColor (Color. r g b a))
      (.fill (RoundRectangle2D$Double. (double 0) (double 0) (double width) (double height) (double corner-arc-width) (double corner-arc-height))))))

(defn create-buffered-image [color width height corner-arc-width corner-arc-height]
  (assert (and (< width 10000)
               (< height 10000))
          (str "Tried to create buffered image larger than 10.000 x 10.000. The requested size was " width " x " height))

  (let [buffered-image (buffered-image/create (max 1
                                                   width)
                                              (max 1
                                                   height))]
    
    (draw (buffered-image/get-graphics buffered-image)
          color width height corner-arc-width corner-arc-height)

    buffered-image))

(defn contains [width height corner-arc-width corner-arc-height x y]
  (.contains (RoundRectangle2D$Double. (double 0) (double 0) (double width) (double height) (double corner-arc-width) (double corner-arc-height))
             (double x)
             (double y)))

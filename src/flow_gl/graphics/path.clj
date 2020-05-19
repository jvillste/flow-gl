(ns flow-gl.graphics.path
  (:require [flow-gl.graphics.buffered-image :as buffered-image])
  (:import (java.awt BasicStroke Color RenderingHints)
           (java.awt.geom GeneralPath)))

(defn draw [graphics color line-width points]
  (let [general-path (GeneralPath. GeneralPath/WIND_EVEN_ODD
                                   (count points))
        first-point (first points)
        [r g b a] (map (fn [color] (float (/ color 255)))
                       color)]

    (.moveTo general-path
               (double (:x first-point))
               (double (:y first-point)))

    (doseq [point points]
      (.lineTo general-path (double (:x point)) (double (:y point))))

    (.setColor graphics (Color. r g b a))

    (.setStroke graphics (BasicStroke. line-width))

    (.draw graphics general-path)))

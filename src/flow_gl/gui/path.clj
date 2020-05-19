(ns flow-gl.gui.path
  (:require [flow-gl.graphics.path :as graphics-path]))

(defn path [color line-width points]
  {:type ::path
   :color color
   :line-width line-width
   :points points
   :draw-function graphics-path/draw
   :image-function-parameter-keys [:color :line-width :points]
   :width (apply max (map :x points))
   :height (apply max (map :y points))})

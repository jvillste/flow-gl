(ns flow-gl.gui.layout-dsl
  (:require (flow-gl.gui [layout :as layout])))

(defn vertically [& contents]
  (layout/->VerticalStack contents))

(defn horizontally [& contents]
  (layout/->HorizontalStack contents))

(defn margin [top right bottom left content]
  (layout/->Margin top right bottom left [content]))



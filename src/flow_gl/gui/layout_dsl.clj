(ns flow-gl.gui.layout-dsl
  (:require (flow-gl.gui [layouts :as layouts])))

(defn vertically [& contents]
  (layouts/->VerticalStack contents))

(defn horizontally [& contents]
  (layouts/->HorizontalStack contents))

(defn margin [top right bottom left content]
  (layouts/->Margin top right bottom left [content]))



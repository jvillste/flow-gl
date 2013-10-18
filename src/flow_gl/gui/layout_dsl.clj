(ns flow-gl.gui.layout-dsl
  (:require (flow-gl.gui [layout :as layout])))

(defn vs [& contents]
  (layout/->VerticalStack contents))

(defn hs [& contents]
  (layout/->HorizontalStack contents))

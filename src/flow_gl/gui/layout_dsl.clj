(ns flow-gl.gui.layout-dsl
  (:require (flow-gl.gui [layouts :as layouts])))

(defn flatten-contents [values]
  (->> values
       (filter (complement nil?))
       (flatten)))

(defn superimpose [& contents]
  (layouts/->Superimpose (flatten-contents contents)))

(defn vertically [& contents]
  (layouts/->VerticalStack (flatten-contents contents)))

(defn horizontally [& contents]
  (layouts/->HorizontalStack (flatten-contents contents)))

(defn margin [top right bottom left content]
  (when content
    (layouts/->Margin top right bottom left [content])))

(defn box [margin outer inner]
  (when (and outer inner)
    (layouts/->Box margin [outer inner])))













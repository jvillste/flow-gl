(ns fungl.component
  (:require [fungl.layouts :as layouts]
            [flow-gl.gui.visuals :as visuals]))

(defn color-overlay [color child]
  (layouts/overlay child
                   (visuals/rectangle-2 :fill-color color)))

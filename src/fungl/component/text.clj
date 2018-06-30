(ns fungl.component.text
  (:require [clojure.java.io :as io]
            [flow-gl.graphics.font :as font]
            [flow-gl.gui.visuals :as visuals]))

(def default-font (font/create (.getPath (io/resource "LiberationSans-Regular.ttf")) 18))
(def default-color [255 255 255 255])

(defn text
  ([value]
   (text value default-color default-font))

  ([value color]
   (text value color default-font))

  ([value color font]
   (visuals/text-area (str value)
                      color
                      font)))

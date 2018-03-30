(ns fungl.component.text
  (:require (flow-gl.graphics [font :as font]
                              [text :as text])
            (flow-gl.gui [visuals :as visuals])
            [clojure.java.io :as io]))

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

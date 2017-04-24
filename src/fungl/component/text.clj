(ns fungl.component.text
  (:require (flow-gl.graphics [font :as font]
                              [text :as text])
            (flow-gl.gui [visuals :as visuals])))

(def default-font (font/create "LiberationSans-Regular.ttf" 18))
(def default-color [255 255 255 255])

(defn text
  ([value]
   (text value default-color default-font))

  ([value color]
   (text value color default-font))

  ([value color font]
   (visuals/text-area color
                      font
                      (str value))))

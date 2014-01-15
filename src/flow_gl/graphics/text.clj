(ns flow-gl.graphics.text
  (:require (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [java.awt Color RenderingHints])
  (:use midje.sweet))

(defn draw [graphics color font text]
  (let [[r g b a] (map float color)]
    (doto graphics
      (.setColor (Color. r g b a))
      (.setFont (font/graphics-font font))
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
      (.drawString text 0 (font/ascent font)))))

(defn create-buffered-image [color font text]
  (let [buffered-image (buffered-image/create (max 1
                                                   (font/width font text))
                                              (max 1
                                                   (font/height font)))]

    (draw (buffered-image/get-graphics buffered-image)
               color
               font
               text)

    buffered-image))

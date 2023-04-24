(ns flow-gl.graphics.text
  (:require [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.font :as font])
  (:import (java.awt Color RenderingHints)
           (java.awt.font LineBreakMeasurer TextAttribute)
           (java.awt.image BufferedImage)
           (java.text AttributedString)))

(defn draw [graphics color font text]
  (let [[r g b a] (map (fn [color] (float (/ color 255))) color)]
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


(defn vector-to-awt-color [color-vector]
  (let [[r g b a] (map (fn [color] (float (/ color 255))) color-vector)]
    (Color. r g b a)))

(defn create-attributed-string [text font color]
  (let [attributed-string (AttributedString. text)]
    (if (not= "" text)
      (doto attributed-string
        (.addAttribute TextAttribute/FONT (font/graphics-font font))
        (.addAttribute TextAttribute/FOREGROUND color)))
    attributed-string))

(comment
  (create-attributed-string "haa"
                            (font/create "LiberationSans-Regular.ttf" 15)
                            (vector-to-awt-color [255 255 255 255])))

(defn rows [line-break-measurer width]
  (.setPosition line-break-measurer 0)
  (loop [position 0
         rows []]
    (if-let [layout (.nextLayout line-break-measurer width)]
      (let [new-position (.getPosition line-break-measurer)]
        (recur new-position
               (conj rows {:layout layout
                           :from position
                           :to new-position} )))

      rows)))

(defn row-length [row]
  (.getCharacterCount (:layout row)))

(defn line-break-measurer [color-vector font text]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
    (.setRenderingHint graphics RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR)
    (LineBreakMeasurer.
     (.getIterator (create-attributed-string text font (vector-to-awt-color color-vector)))
     (.getFontRenderContext graphics))))


(defn rows-for-text [color-vector font string width]
  (rows (line-break-measurer color-vector
                             font
                             string)
        width))

(comment

  (layouts-for-text [255 255 255 255]
                    (font/create "LiberationSans-Regular.ttf" 15)
                    "a b c"
                    10))

(defn layout-height [layout]
  (+ (.getDescent layout)
     (.getAscent layout)
     (.getLeading layout)))

(defn row-height [row]
  (layout-height (:layout row)))

(defn rows-size [rows]
  (loop [height 0
         width 0
         rows rows]
    (if-let [layout (:layout (first rows))]
      (recur (+ height
                (.getDescent layout)
                (.getAscent layout)
                (.getLeading layout))
             (max width (.getVisibleAdvance layout))
             (rest rows))
      {:width width
       :height height})))


(comment
  (rows-size (rows (line-break-measurer [255 255 255 255]
                                        (font/create "LiberationSans-Regular.ttf" 15)
                                        "a b c")
                   100)))

(defn draw-rows [graphics rows]
  (loop [y 0
         rows rows]
    (when-let [layout (:layout (first rows))]
      (do (.draw layout graphics 0 (+ y (.getAscent layout))))
      (recur (+ y
                (.getAscent layout)
                (.getDescent layout)
                (.getLeading layout))
             (rest rows)))))


(defn create-buffered-image-for-rows [rows]
  (let [size (rows-size rows)
        buffered-image (buffered-image/create (max 1
                                                   (:width size))
                                              (max 1
                                                   (:height size)))]


    (when rows
      (draw-rows (buffered-image/get-graphics buffered-image)
                 rows))

    buffered-image))

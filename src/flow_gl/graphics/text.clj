(ns flow-gl.graphics.text
  (:require (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [java.awt Color RenderingHints]
           [java.awt.font LineBreakMeasurer TextAttribute]
           [java.text AttributedString]
           [java.awt.image BufferedImage]))

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
    (doto attributed-string
      (.addAttribute TextAttribute/FONT (font/graphics-font font))
      (.addAttribute TextAttribute/FOREGROUND color))
    attributed-string))

(comment
  (create-attributed-string "haa"
                            (font/create "LiberationSans-Regular.ttf" 15)
                            (vector-to-awt-color [255 255 255 255])))

(defn layouts [line-break-measurer width]
  (.setPosition line-break-measurer 0)
  (loop [layouts []]
    (if-let [layout (.nextLayout line-break-measurer width)]
      (recur (conj layouts layout))
      layouts)))


(defn line-break-measurer [color-vector font text]
  (LineBreakMeasurer.
   (.getIterator (create-attributed-string text font (vector-to-awt-color color-vector)))
   (.getFontRenderContext (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB)))))


(defn layouts-for-text [color-vector font string width]
  (layouts (line-break-measurer color-vector
                                font
                                string)
           width))

(comment
  (count (layouts (line-break-measurer [255 255 255 255]
                                       (font/create "LiberationSans-Regular.ttf" 15)
                                       "a b c")
                  10)))

(defn layouts-size [layouts]
  (loop [height 0
         width 0
         layouts layouts]
    (if-let [layout (first layouts)]
      (let [bounds (.getBounds layout)]
        (recur (+ height
                  (.getHeight bounds))
               (max width (.getWidth bounds))
               (rest layouts)))
      
      {:width width
       :height height})))

(comment
  (layouts-size (layouts (line-break-measurer [255 255 255 255]
                                              (font/create "LiberationSans-Regular.ttf" 15)
                                              "a b c")
                         10)))

(defn draw-layouts [graphics layouts]
  (.setRenderingHint graphics RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR)
  (loop [y 0
         layouts layouts]
    (when-let [layout (first layouts)]
      (do (prn "ascent" (.getAscent layout))
          (.draw layout graphics 0 (- (+ y (.getAscent layout))
                                   3)))
      (recur (+ y
                (.getHeight (.getBounds layout)))
             (rest layouts)))))


(defn create-buffered-image-for-layouts [layouts]
  
  (let [layouts-size (layouts-size layouts)
        buffered-image (buffered-image/create (max 1
                                                   (:width layouts-size))
                                              (max 1
                                                   (:height layouts-size)))]


    (when layouts
      (draw-layouts (buffered-image/get-graphics buffered-image)
                    layouts))

    buffered-image))

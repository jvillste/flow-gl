(ns flow-gl.graphics.font
  (:import (java.awt Font)
           (java.awt.font TextLayout)
           (java.awt.image BufferedImage))
  (:require [clojure.java.io :as io]))

(def loaded-fonts (atom {}))

(defn create [ttf-file-name size]
  (swap! loaded-fonts
         (fn [loaded-fonts]
           (if (contains? loaded-fonts [ttf-file-name size])
             loaded-fonts
             (assoc loaded-fonts
                    [ttf-file-name size]
                    (let [font (-> (Font/createFont Font/TRUETYPE_FONT (io/input-stream ttf-file-name))
                                   (.deriveFont (float size)))
                          graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]

                      (.setFont graphics font)

                      {:font font
                       :font-metrics (.getFontMetrics graphics)})))))

  (@loaded-fonts [ttf-file-name size]))

(defn width [font text] (.stringWidth (:font-metrics font) text))

(defn height [font] (+ (.getMaxAscent (:font-metrics font))
                       (.getMaxDescent (:font-metrics font))))

(defn ascent [font] (.getMaxAscent (:font-metrics font)))

(defn graphics-font [font] (:font font))

(defn character-index-at-position [font text position]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))
        text-layout (TextLayout. text font (.getFontRenderContext graphics))
        hit-info (.hitTestChar text-layout position 1)]
    (.getCharIndex hit-info)))

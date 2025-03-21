(ns flow-gl.graphics.font
  (:import (java.awt Font)
           (java.awt.font TextLayout)
           (java.awt.image BufferedImage)
           java.awt.GraphicsEnvironment)
  (:require [clojure.java.io :as io]
            [medley.core :as medley]))

(comment
  (.deriveFont (medley/find-first #(= "ArialMT" (.getName %))
                                  (.getAllFonts (GraphicsEnvironment/getLocalGraphicsEnvironment)))
               (float 20))
  )

(defn available-names []
  (map #(.getName %)
       (.getAllFonts (GraphicsEnvironment/getLocalGraphicsEnvironment))))

(defn font-with-metrics [font]
  (let [^java.awt.Graphics2D graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]

    (.setFont graphics font)

    {:font font
     :font-metrics (.getFontMetrics graphics)}))

(defn create-by-name [name size]
  (font-with-metrics (.deriveFont (medley/find-first #(= name (.getName %))
                                                     (.getAllFonts (GraphicsEnvironment/getLocalGraphicsEnvironment)))
                                  (float size))))

(defn built-in-font [& [{:keys [name size style]
                         :or {name :dialog
                              size 40
                              style :plain}}]]

  (font-with-metrics (Font. (get {:dialog "Dialog"
                                  :dialog-input "DialogInput"
                                  :monospaced "Monospaced"
                                  :serif "Serif"
                                  :sans-serif "SansSerif"}
                                 name)
                            (get {:plain Font/PLAIN
                                  :italic Font/ITALIC
                                  :bold Font/BOLD}
                                 style)
                            size)))

(defn derive [font size]
  (.deriveFont font
               (float size)))

(def loaded-fonts (atom {}))

(defn create [ttf-file-name size]
  (swap! loaded-fonts
         (fn [loaded-fonts]
           (if (contains? loaded-fonts [ttf-file-name size])
             loaded-fonts
             (assoc loaded-fonts
                    [ttf-file-name size]
                    (font-with-metrics (-> (Font/createFont Font/TRUETYPE_FONT (io/input-stream ttf-file-name))
                                           (.deriveFont (float size))))))))

  (@loaded-fonts [ttf-file-name size]))

(defn width [font text] (.stringWidth ^java.awt.FontMetrics (:font-metrics font) text))

(defn height [font] (+ (.getMaxAscent (:font-metrics font))
                       (.getMaxDescent (:font-metrics font))))

(defn ascent [font] (.getMaxAscent (:font-metrics font)))

(defn graphics-font [font] (:font font))

(defn character-index-at-position [font text position]
  (let [^java.awt.Graphics2D graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))
        text-layout (TextLayout. text font (.getFontRenderContext graphics))
        hit-info (.hitTestChar text-layout position 1)]
    (.getCharIndex hit-info)))

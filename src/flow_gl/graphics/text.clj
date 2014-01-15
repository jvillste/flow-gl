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
      (.drawString text 0 (font/ascent font))
      )))


;; TextLayout layout = new TextLayout(label, font, frc);
;;         Rectangle r = layout.getPixelBounds(null, 0, 0);
;;         System.out.println(r);
;;         BufferedImage bi = new BufferedImage(
;;             r.width + 1, r.height + 1,
;;             BufferedImage.TYPE_INT_ARGB);
;;         Graphics2D g2d = (Graphics2D) bi.getGraphics();
;;         g2d.setColor(Color.blue);
;;         layout.draw(g2d, 0, -r.y);
;;         g2d.dispose();

#_(fact (draw (buffered-image/get-graphics (buffered-image/create 100 100))
            [1 1 1 1 1]
            (font/create "LiberationSans-Regular.ttf" 20)
            "hello"))

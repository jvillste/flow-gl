(ns examples.java2d
  (:require [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layout :as layout]
            [fungl.layouts :as layouts]))

(defn create-scene-graph []
  (layouts/vertically-2 {:margin 10}
                        (assoc (visuals/rectangle-2 :fill-color [255 255 255 255])
                               :height 50)
                        (visuals/text "haa" {:color [0 0 0 255]})
                        (assoc (visuals/rectangle-2 :fill-color [50 10 100 255])
                               :height 50)))

(defn start []
  (application/start-window #'create-scene-graph))

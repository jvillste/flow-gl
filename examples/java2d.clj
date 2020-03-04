(ns examples.java2d
  (:require [flow-gl.gui.visuals :as visuals]
            [flow-gl.swing.window :as window]
            [fungl.application :as application]
            [fungl.layouts :as layouts]
            [fungl.swing.root-renderer :as root-renderer]))

(defn create-scene-graph [width height]
  (-> (layouts/vertically-2 {:margin 10}
                            (assoc (visuals/rectangle-2 :fill-color [255 255 255 255])
                                   :height 50)
                            (visuals/text "haa" {:color [0 0 0 255]})
                            (assoc (visuals/rectangle-2 :fill-color [50 10 100 255])
                                   :height 50))
      (application/do-layout width height)
      (assoc :render root-renderer/root-renderer)))

(defn start []
  (application/start-window #'create-scene-graph
                            :window
                            (window/create :width 600
                                           :hegiht 600)))

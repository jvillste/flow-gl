(ns fungl.swing.root-renderer
  (:require [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.cache :as cache]
            [fungl.render :as render])
  (:import java.awt.Color
           [java.awt.geom AffineTransform RoundRectangle2D$Double]))

(defn nodes-in-view [scene-graph width height]
  (filter (fn [node]
            (scene-graph/intersects? {:x 0 :y 0 :width width :height height}
                                     node))
          (cache/call! scene-graph/leaf-nodes scene-graph)))

(defn root-renderer [scene-graph graphics]
  (let [transform (AffineTransform.)]
    (doseq [node (filter :draw-function (nodes-in-view scene-graph (:width scene-graph) (:height scene-graph)))]
      (.setToTranslation transform (:x node) (:y node))
      (.setTransform graphics transform)
      (apply (:draw-function node)
             graphics
             (render/image-function-parameters node)))))

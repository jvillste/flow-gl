(ns fungl.swing.root-renderer
  (:require [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.cache :as cache]
            [fungl.render :as render]
            [flow-gl.graphics.path :as path])
  (:import java.awt.geom.AffineTransform
           [java.awt RenderingHints]))

(defn nodes-in-view [scene-graph width height]
  (filter (fn [node]
            (scene-graph/intersects? {:x 0 :y 0 :width width :height height}
                                     node))
          (cache/call! scene-graph/leaf-nodes scene-graph)))

(defn root-renderer [scene-graph graphics]
  ;; (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (let [transform (AffineTransform.)]
    (doseq [node (filter :draw-function (nodes-in-view scene-graph (:width scene-graph) (:height scene-graph)))]
      (.setToTranslation transform (:x node) (:y node))
      (.setTransform graphics transform)
      (apply (:draw-function node)
             graphics
             (render/image-function-parameters node)))))

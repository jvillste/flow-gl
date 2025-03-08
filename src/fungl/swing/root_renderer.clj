(ns fungl.swing.root-renderer
  (:require
   [flow-gl.graphics.rectangle :as rectangle]
   [fungl.color :as color]
   [fungl.render :as render])
  (:import
   (java.awt.geom AffineTransform)))

(defn render-nodes [^java.awt.Graphics2D graphics nodes & [{:keys [color-nodes?] :or {color-nodes? false}}]]
  #_(println "render-nodes" (count nodes)
             ;;           (first (call-stack/callers))
             )

  (let [transform (AffineTransform.)]
    (doseq [node nodes]
      (.setToTranslation transform (:x node) (:y node))
      (.setTransform graphics transform)

      ;; (prn 'drawing (:draw-function node) (:id node))

      (apply (:draw-function node)
             graphics
             (render/image-function-parameters node))

      (when color-nodes?
        (rectangle/fill graphics
                        (concat (color/hsl-to-rgb (rand-int 360)
                                                  1
                                                  0.5)
                                [40])
                        (:width node)
                        (:height node)
                        0
                        0)))))

(ns fungl.swing.root-renderer
  (:require
   [flow-gl.graphics.buffered-image :as buffered-image]
   [flow-gl.gui.scene-graph :as scene-graph]
   [fungl.cache :as cache]
   [fungl.render :as render])
  (:import
   (java.awt.geom AffineTransform)
   (java.awt Color)))

(defn render-nodes [graphics nodes]
  (let [transform (AffineTransform.)]
    (doseq [node nodes]
      (.setToTranslation transform (:x node) (:y node))
      (.setTransform graphics transform)

      ;; (prn 'drawing
      ;;      (:z node)
      ;;      (:id node)
      ;;      (:draw-function node)) ;; TODO remove me

      (apply (:draw-function node)
             graphics
             (render/image-function-parameters node)))))

(defn render-scene-graph [graphics scene-graph]
  #_(prn 'render-scene-graph #_scene-graph
         (:id scene-graph)
         (count (scene-graph/leaf-nodes scene-graph))) ;; TODO: remove me

  (doto graphics
    #_(.setColor (Color. 255 255 255 255))
    (.setColor (Color. 0 0 0 255))
    (.fillRect 0 0 5000 5000))

  (render-nodes graphics
                (filter :draw-function
                        (scene-graph/nodes-in-view scene-graph
                                                   (:width scene-graph)
                                                   (:height scene-graph)))))

(defn render-to-buffered-image [bounding-box leaf-nodes]
  (let [buffered-image (buffered-image/create (min (:width bounding-box)
                                                   10000)
                                              (min (:height bounding-box)
                                                   10000))]

    (render-nodes (buffered-image/get-graphics buffered-image)
                  (map (fn [node]
                         (-> node
                             (update :x #(- % (:x bounding-box)))
                             (update :y #(- % (:y bounding-box)))))
                       leaf-nodes))

    buffered-image))

(ns examples.3d
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [atom-registry :as atom-registry]
                   [application :as application]
                   [layouts :as layouts]
                   [layout :as layout]
                   [cache :as cache]
                   [handler :as handler])
            (fungl.component [text :as text-component])
            [clojure.core.matrix :as matrix]
            (flow-gl.gui 
             [keyboard :as keyboard]
             [visuals :as visuals]
             [quad-renderer :as quad-renderer]
             [render-target-renderer :as render-target-renderer]
             [animation :as animation]
             [stateful :as stateful])
            [flow-gl.opengl.math :as math]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [quad :as quad]
                                 [texture :as texture]
                                 [render-target :as render-target]
                                 [multicolor-triangle-list :as multicolor-triangle-list])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]
                              [text :as text]))
  (:import  [javax.imageio ImageIO]
            [java.io File])
  (:use clojure.test))



(handler/def-handler-creator create-renderer [phase] [scene-graph gl]
  (let [{:keys [width height]} scene-graph
        render-target-atom (atom-registry/get! [:render-target width height]  (render-target/atom-specification width height gl))
        multicolor-triangle-list-atom (atom-registry/get! :multicolor-triangle-list {:create (fn [] (multicolor-triangle-list/create gl :triangles 3))
                                                                                     :delete (fn [state] (multicolor-triangle-list/delete state gl))})]
    
    #_(swap! multicolor-triangle-list-atom
             multicolor-triangle-list/set-size
             (:width scene-graph)
             (:height scene-graph)
             gl)
    (render-target/render-to @render-target-atom gl
                             (opengl/clear gl 0 0 0 1)
                             (swap! multicolor-triangle-list-atom
                                    multicolor-triangle-list/set-projection-matrix
                                    (math/core-matrix-to-opengl-matrix #_(math/projection-matrix-2d (:width scene-graph)
                                                                                                    (:height scene-graph))
                                                                       #_(math/projection-matrix-2d 1.0
                                                                                                    1.0)
                                                                       #_(math/projection-matrix-3d -100.0 100.0 0.0 (:width scene-graph) (:height scene-graph) 0.0)
                                                                       
                                                                       #_(math/projection-matrix-3d -1.0 1.0 -1.0 1.0 -1.0 1.0)
                                                                       (let [aspect-ratio 1.0
                                                                             near 10
                                                                             far 100
                                                                             field-of-view (/ Math/PI 2)
                                                                             top (math/top field-of-view near)
                                                                             right (* top aspect-ratio)
                                                                             left (- (* top aspect-ratio))
                                                                             bottom (- top)]
                                                                         (matrix/mmul #_(math/scaling-matrix 0.5 0.5)
                                                                                      (math/translation-matrix 0.0 0.0 0.0 #_phase #_(animation/linear-mapping phase 1.0 3.0))
                                                                                      #_(math/translation-matrix 0.0 0.0 50.0)
                                                                                      #_(math/x-rotation-matrix (* (animation/linear-mapping phase 0.0 360.0) (/ Math/PI 180)))
                                                                                      #_(math/z-rotation-matrix (* (animation/linear-mapping phase 0.0 360.0) (/ Math/PI 180)))
                                                                                      #_(math/projection-matrix-3d-orthogonal 10.0 100.0 -1 1 1 -1)
                                                                                      #_(math/projection-matrix-3d-orthogonal near far left right top bottom)
                                                                                      #_(math/projection-matrix-3d 10.0 100.0 -10.0 10.0 10.0 -10.0))))
                                    gl)

                             (swap! multicolor-triangle-list-atom
                                    multicolor-triangle-list/render-coordinates
                                    [0.0 0.0 1.0
                                     0 (animation/linear-mapping phase 1.0 1.0) 1.0
                                     1.0 1.0 1.0] 
                                    [1 0 0 1
                                     0 1 0 1
                                     0 0 1 1]
                                    gl))
    
    
    
    

    (assoc (select-keys scene-graph [:x :y :width :height])
           :texture-id (:texture @render-target-atom)
           :texture-hash (hash scene-graph))))

(defn canvas [width height phase]
  
  {:width width
   :height height
   :id :canvas
   :render (create-renderer phase)})

(defn create-scene-graph [width height]
  (animation/swap-state! animation/start-if-not-running :animation)
  (application/do-layout (let [phase (animation/ping-pong 5
                                                          (animation/phase! :animation))]
                           (layouts/vertically (canvas width (- height 30)
                                                       phase)
                                               (text-component/text (float phase))))
                         
                         width height))

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))

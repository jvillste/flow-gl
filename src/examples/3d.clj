(ns examples.3d
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [atom-registry :as atom-registry]
                   [application :as application]
                   [layouts :as layouts]
                   [layout :as layout]
                   [cache :as cache]
                   [handler :as handler])
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
  (let [multicolor-triangle-list-atom (atom-registry/get! :multicolor-triangle-list {:create (fn [] (multicolor-triangle-list/create gl :triangles 3))
                                                                                     :delete (fn [state] (multicolor-triangle-list/delete state gl))})]
    (opengl/clear gl 0 0 0 1)
    #_(swap! multicolor-triangle-list-atom
             multicolor-triangle-list/set-size
             (:width scene-graph)
             (:height scene-graph)
             gl)

    (swap! multicolor-triangle-list-atom
           multicolor-triangle-list/set-projection-matrix
           (math/core-matrix-to-opengl-matrix #_(math/projection-matrix-2d (:width scene-graph)
                                                                           (:height scene-graph))
                                              (math/projection-matrix-3d -1.0 1.0 0.0 (:width scene-graph) (:height scene-graph) 0.0))
           gl)
    
    (swap! multicolor-triangle-list-atom
           multicolor-triangle-list/render-coordinates
           [0.0 0.0 0.0
            100 100 0.0
            0 (animation/linear-mapping phase 100 50) 0.0] 
           [1 0 0 1
            0 1 0 1
            0 0 1 1]
           gl))

  scene-graph)

(defn create-scene-graph [width height]
  ;; (animation/swap-state! animation/set-wake-up 1000)
  (animation/swap-state! animation/start-if-not-running :animation)
  
  {:x 0
   :y 0
   :width width
   :height height
   :id :canvas
   :render (create-renderer (animation/ping-pong 1
                                                 (animation/phase! :animation)))})

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))

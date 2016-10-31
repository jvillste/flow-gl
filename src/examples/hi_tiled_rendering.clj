(ns examples.hi-tiled-rendering
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [render-target-renderer :as render-target-renderer]
                         [animation :as animation])))

(defn box-in-a-box [id phase color]
  {:children [(assoc (visuals/rectangle color 0 0)
                     :x 0
                     :y 0
                     :width 200
                     :height 200)
              (assoc (visuals/rectangle [255 255 255 255] 0 0)
                     :x (+ (- 100) (animation/linear-mapping phase
                                                             0 300))
                     :y (+ (- 100) (animation/linear-mapping phase
                                                             0 300))
                     :width 100
                     :height 100)]
   :width 200
   :height 200
   :id id
   :renderers [(assoc (render-target-renderer/renderer [(assoc quad-renderer/renderer
                                                               :id [id :render-target-quad-renderer])])
                      :id [id :render-target])]})

(defn create-scene-graph [width height]
  (animation/swap-state! animation/start-if-not-running :offset)
  (let [phase (animation/ping-pong 3
                                   (animation/phase! :offset
                                                     nil))]
    {:children [(assoc (box-in-a-box :box-1
                                     phase
                                     [255 155 155 255])
                       :x 10
                       :y 10)
                (assoc (box-in-a-box :box-2
                                     (mod (+ 0.3 phase)
                                          1)
                                     [255 155 255 255])
                       :x 100
                       :y 250)]
     :x 0
     :y 0
     :width width
     :height height
     :renderers [(assoc quad-renderer/renderer
                        :id :root-renderer)]}))

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))

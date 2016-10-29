(ns examples.hi-renderers-in-scene-graph
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [render-target-renderer :as render-target-renderer]
                         [animation :as animation])))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/start-if-not-running :offset)
  (let [offset (animation/linear-mapping (animation/ping-pong 3
                                                              (animation/phase! :offset
                                                                                nil))
                                         0 300)]
    
    {:children [(assoc (visuals/rectangle [255 155 155 255] 0 0)
                       :x 0
                       :y 0
                       :width 200
                       :height 200)
                (assoc (visuals/rectangle [255 255 255 255] 0 0)
                       :x (+ (- 100) offset)
                       :y (+ (- 100) offset)
                       :width 100
                       :height 100)]
     :x 10
     :y 10
     :width 200
     :height 200
     :id :root
     :renderers [(assoc (render-target-renderer/renderer [(assoc quad-renderer/renderer
                                                                 :id :render-target-quad-renderer)])
                        :id :render-target)
                 (assoc quad-renderer/renderer
                        :id :quad-renderer-1)]}))

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))

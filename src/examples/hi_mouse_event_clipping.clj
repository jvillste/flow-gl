(ns examples.hi-tiled-rendering
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [tiled-renderer :as tiled-renderer]
                         [render-target-renderer :as render-target-renderer]
                         [animation :as animation]
                         [layouts :as layouts]
                         [scene-graph :as scene-graph])
            (flow-gl.graphics [font :as font])))


(def state (atom {}))

(defn stateful-rectangle [id]
  (let [rectangle-state (get @state id)]
    (-> (visuals/rectangle (if (:mouse-over rectangle-state)
                             (if (:mouse-down rectangle-state)
                               [255 0 0 150]
                               [255 255 255 150])
                             [0 255 255 150])
                           80 80)
        (assoc :width 200
               :height 200
               :id id
               :mouse-event-handler (fn [node event]
                                      (swap! state update-in [id]
                                             (fn [rectangle-state]
                                               (case (:type event)
                                                 :mouse-entered (assoc rectangle-state :mouse-over true)
                                                 :mouse-left (assoc rectangle-state
                                                                    :mouse-over false
                                                                    :mouse-down false)
                                                 :mouse-pressed (assoc rectangle-state
                                                                       :mouse-down true)
                                                 :mouse-released (assoc rectangle-state
                                                                        :mouse-down false)
                                                 rectangle-state)))
                                      
                                      event)))))


(defn create-scene-graph [width height]
  (let [margin 100
        mask {:children [(assoc (visuals/rectangle [255 0 0 255] 40 40)
                                :x 40
                                :y 40
                                :width 100
                                :height 50)]
              :x 0
              :y 0}]
    {:children [{:children [(assoc (stateful-rectangle :rectangle-1)
                                   :x 0
                                   :y 0)
                            mask]
                 :x margin
                 :y margin
                 :width (- width (* 2 margin))
                 :height (- height (* 2 margin))
                 :clip-mouse-events (fn [node x y]
                                      (or (not (scene-graph/in-coordinates? node x y))
                                          (some (fn [mask-node]
                                                  (let [local-x (- x (:x node))
                                                        local-y (- y (:y node))]
                                                    (scene-graph/hits? mask-node
                                                                 local-x local-y)))
                                                (scene-graph/leaf-nodes mask))))
                 :renderers [(assoc (render-target-renderer/renderer (assoc quad-renderer/renderer
                                                                            :id :render-target-quad-renderer))
                                    :id :render-target)]}]
     :x 0 :y 0 :width width :height height
     :renderers [(assoc quad-renderer/renderer
                        :id :root-quad-renderer)]}))


(defn start []
  (do (spec-test/instrument)
      (spec/check-asserts true))
  
  #_(do (spec-test/unstrument)
        (spec/check-asserts false))

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :target-frame-rate 30)))))

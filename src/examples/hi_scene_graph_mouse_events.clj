(ns examples.hi-scene-graph-mouse-events
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals])))
  
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
  {:x 50 :y 50
   :children (concat [(assoc (stateful-rectangle :rectangle-1)
                             :x 0
                             :y 0)
                      (assoc (stateful-rectangle :rectangle-2)
                             :x 100
                             :y 100)])})

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  (application/start-window create-scene-graph)
  #_(.start (Thread. (fn []
                       (start-window)))))

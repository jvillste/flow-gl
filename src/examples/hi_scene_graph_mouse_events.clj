(ns examples.hi-scene-graph-mouse-events
  (:require [clojure.spec.test :as spec-test]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            [clojure.core.async :as async]
            (flow-gl.gui [window :as window]
                         [quad-renderer :as quad-renderer]
                         [scene-graph :as scene-graph]
                         [mouse :as mouse])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target]))
  (:use flow-gl.utils
        clojure.test))



(defn draw-rectangle [width height color]
  (rectangle/create-buffered-image color width height 10 10))

(defn rectangle [x y width height color]
  {:x x
   :y y
   :width width
   :height height
   :image-function draw-rectangle
   :parameters [width height color]})



(def state (atom {}))

(defn stateful-rectangle [id x y]
  (-> (rectangle x y 200 200 (if (get-in @state [:mouse-over id])
                               (if (get-in @state [:mouse-down id])
                                 [255 0 0 150]
                                 [255 255 255 150])
                               [0 255 255 150]))
      (assoc :id id
             :mouse-event-handler (fn [event]
                                    (case (:type event)
                                      :mouse-entered (swap! state assoc-in [:mouse-over id] true)
                                      :mouse-left (swap! state (fn [state]
                                                                 (-> state
                                                                     (assoc-in [:mouse-over id] false)
                                                                     (assoc-in [:mouse-down id] false))))
                                      :mouse-pressed (swap! state assoc-in [:mouse-down id] true)
                                      :mouse-released (swap! state assoc-in [:mouse-down id] false)
                                      nil)
                                    event))))

(defn nodes []
  {:x 50 :y 50
   :children (concat [(stateful-rectangle :rectangle-1 0 0)
                      (stateful-rectangle :rectangle-2 100 100)])})

(defn start-window []
  (let [window-width 400
        window-height 400
        window (jogl-window/create window-width
                                   window-height
                                   :close-automatically true)
        event-channel (window/event-channel window)]

    (loop [flow-gl-state {:quad-renderer (window/with-gl window gl (quad-renderer/create gl))
                          :previous-mouse-event-handlers-under-mouse {}}]
      
      (when (window/visible? window)
        (recur (try
                 
                 (let [scene-graph (nodes)
                       quad-renderer (window/with-gl window gl
                                       (opengl/clear gl 0 0 0 1)

                                       (quad-renderer/draw (:quad-renderer flow-gl-state)
                                                           (scene-graph/leave-nodes scene-graph)
                                                           window-width
                                                           window-height
                                                           gl))]
                   (window/swap-buffers window)

                   (let [event (async/<!! event-channel)
                         previous-mouse-event-handlers-by-id (if (= :mouse (:source event))
                                                               (let [mouse-event-handler-nodes-under-mouse (mouse/mouse-event-handler-nodes-in-coodriantes scene-graph
                                                                                                                                                           (:x event)
                                                                                                                                                           (:y event))]
                                                                 (mouse/call-mouse-event-handlers mouse-event-handler-nodes-under-mouse
                                                                                                  event)
                                                                 (mouse/send-mouse-over-events (:previous-mouse-event-handlers-under-mouse flow-gl-state)
                                                                                               mouse-event-handler-nodes-under-mouse))
                                                               (:previous-mouse-event-handlers-under-mouse flow-gl-state))]
                     {:quad-renderer quad-renderer
                      :previous-mouse-event-handlers-under-mouse previous-mouse-event-handlers-by-id}))
                 
                 (catch Throwable e
                   (.printStackTrace e *out*)
                   (window/close window)
                   (throw e))))))
    (println "exiting")))


(defn start []
  (spec-test/instrument)
  #_(start-window)
  (.start (Thread. (fn []
                     (start-window)))))

;; TODO: How to hit test round corners in rectangle?





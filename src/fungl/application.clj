(ns fungl.application
  (:require [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            (fungl [renderer :as renderer])
            (flow-gl.gui [window :as window]
                         [layout :as layout]
                         [quad-renderer :as quad-renderer]
                         [scene-graph :as scene-graph]
                         [mouse :as mouse]
                         [keyboard :as keyboard]
                         [stateful :as stateful]
                         [animation :as animation])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window])))

(defn do-layout [scene-graph window-width window-height]
  (-> scene-graph
      (assoc :x 0
             :y 0
             :available-width window-width
             :available-height window-height)
      (layout/do-layout)))

(defn create-event-handling-state []
  (conj (stateful/state-bindings)
        (mouse/state-bindings)
        (keyboard/state-bindings)
        (animation/state-bindings)))

(defn create-render-state [gl]
  (stateful/state-bindings))

(defn render [gl scene-graph]

  (renderer/apply-renderers! (assoc scene-graph
                                    :renderers (if-let [renderers (:renderers scene-graph)]
                                                 renderers
                                                 [(assoc quad-renderer/renderer
                                                         :id ::root-renderer)]))
                             gl)
  
  #_(let [{:keys [width height]} (opengl/size gl)]
      (quad-renderer/draw! (scene-graph/leave-nodes scene-graph)
                           width height
                           gl)))

(defn handle-event [scene-graph event]
  (when (= :mouse
           (:source event))
    (mouse/handle-mouse-event! scene-graph event))
  
  (when (= :keyboard
           (:source event))
    (keyboard/handle-keyboard-event! scene-graph event)))

(defn create-window []
  (jogl-window/create 400 400
                      :close-automatically true))

(defn read-events [event-channel target-frame-rate]
  (animation/swap-state! animation/adjust-sleep-time-according-to-target-frames-per-second
                         target-frame-rate
                         (System/currentTimeMillis))



  
  (let [events (csp/drain event-channel
                          (:sleep-time @animation/state-atom))
        events (if (empty? events)
                 [{:type :wake-up}]
                 events)]
    
    (animation/swap-state! animation/set-time-in-milliseconds (System/currentTimeMillis))    
    (animation/swap-state! animation/remove-wake-up)
    
    events))

(defn start-window [create-scene-graph & {:keys [window
                                                 target-frame-rate
                                                 ;;                                                  handle-event
                                                 ;;                                                  render
                                                 ;;                                                  create-event-handling-state
                                                 ;;                                                  create-render-state
                                                 ;;                                                  read-events
                                                 ]
                                          :or {window (create-window)
                                               target-frame-rate 60
                                               ;;                                                handle-event handle-event
                                               ;;                                                read-events read-events
                                               ;;                                                render render
                                               ;;                                                create-event-handling-state create-event-handling-state
                                               ;;                                                create-render-state create-render-state
                                               }}]

  (let [event-channel (window/event-channel window)
        renderable-scene-graph-channel (async/chan)]
    
    ;; use async/thread to inherit bindings such as flow-gl.debug/dynamic-debug-channel
    (async/thread
      (try (with-bindings (window/with-gl window gl
                            (create-render-state gl))
             (loop []
               (let [scene-graph (async/<!! renderable-scene-graph-channel)]
                 (when scene-graph
                   (window/with-gl window gl
                     (render gl scene-graph))
                   (window/swap-buffers window)
                   (recur)))))

           (println "exiting render loop")
           
           (catch Throwable e
             (.printStackTrace e *out*)
             (window/close window)
             (throw e))))
    
    
    (try
      (with-bindings (create-event-handling-state)

        
        
        (loop [scene-graph (create-scene-graph (window/width window)
                                               (window/height window))]
          (when (window/visible? window)


            (let [window-width (window/width window)
                  window-height (window/height window)
                  scene-graph (loop [events (read-events event-channel target-frame-rate)
                                     scene-graph scene-graph]

                                
                                (if-let [event (first events)]
                                  (do (handle-event scene-graph event)
                                      (recur (rest events)
                                             (create-scene-graph window-width
                                                                 window-height)))
                                  scene-graph))]

              (async/>!! renderable-scene-graph-channel
                         scene-graph)
              (recur scene-graph)))))

      (println "exiting event handling loop")

      (catch Throwable e
        (.printStackTrace e *out*)
        (window/close window)
        (throw e))
      (finally
        (async/close! renderable-scene-graph-channel)))))


(ns fungl.application
  (:require [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            (fungl [renderer :as renderer]
                   [cache :as cache]
                   [value-registry :as value-registry]
                   [atom-registry :as atom-registry]
                   [layout :as layout])
            (flow-gl.gui [window :as window]
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
  (conj (stateful/state-bindings :delete-after-calls 50)
        (mouse/state-bindings)
        (keyboard/state-bindings)
        (animation/state-bindings)
        (cache/state-bindings)
        (value-registry/state-bindings)))

(defn create-render-state [gl]
  (conj (stateful/state-bindings :delete-after-calls 500)
        (cache/state-bindings)
        (value-registry/state-bindings)))

(defn render [gl scene-graph]

  (renderer/apply-renderers! (assoc scene-graph
                                    :render (if-let [render (:render scene-graph)]
                                              render
                                              (fn [scene-graph gl]
                                                (opengl/clear gl 0 0 0 1)
                                                (let [quad-renderer-atom (atom-registry/get! ::root-renderer (quad-renderer/atom-specification gl))]
                                                  (quad-renderer/render quad-renderer-atom gl scene-graph)))))
                             gl)
  
  #_(let [{:keys [width height]} (opengl/size gl)]
      (quad-renderer/draw! (scene-graph/leave-nodes scene-graph)
                           width height
                           gl)))

(defn handle-event [scene-graph event]
  (do ;;taoensso.timbre.profiling/profile :info :handle-event
    (when (= :mouse
             (:source event))
      (mouse/handle-mouse-event! event))

    (when (= :keyboard
             (:source event))
      (keyboard/handle-keyboard-event! scene-graph event))))

(defn handle-new-scene-graph [scene-graph]
  (keyboard/handle-new-scene-graph! scene-graph)
  (mouse/handle-new-scene-graph! scene-graph))

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

(defmacro with-profiling [do-profile id & body]
  `(if ~do-profile
     (taoensso.timbre.profiling/profile :info ~id
                                        ~@body)
     (do ~@body)))

(defn start-window [create-scene-graph & {:keys [window
                                                 target-frame-rate
                                                 handle-event
                                                 profiling
                                                 ;;                                                  render
                                                 ;;                                                  create-event-handling-state
                                                 ;;                                                  create-render-state
                                                 ;;                                                  read-events
                                                 ]
                                          :or {target-frame-rate 60
                                               handle-event handle-event
                                               profiling false
                                               ;;                                                read-events read-events
                                               ;;                                                render render
                                               ;;                                                create-event-handling-state create-event-handling-state
                                               ;;                                                create-render-state create-render-state
                                               }}]

  (let [window (or window (create-window))
        event-channel (window/event-channel window)
        renderable-scene-graph-channel (async/chan)]
    
    ;; use async/thread to inherit bindings such as flow-gl.debug/dynamic-debug-channel
    (async/thread
      (try (with-bindings (window/with-gl window gl
                            (create-render-state gl))
             (loop []
               (let [scene-graph (async/<!! renderable-scene-graph-channel)]
                 (when scene-graph
                   (window/with-gl window gl
                     (with-profiling profiling :render
                       (render gl scene-graph)
                       (value-registry/delete-unused-values! 500)))
                   (window/swap-buffers window)
                   (recur)))))

           (println "exiting render loop")
           
           (catch Throwable e
             (.printStackTrace e *out*)
             (window/close window)
             (throw e))))
    

    (async/thread
      (try
        (with-bindings (create-event-handling-state)
          (let [initial-scene-graph (create-scene-graph (window/width window)
                                                        (window/height window))]
            (handle-new-scene-graph initial-scene-graph)
            (loop [scene-graph initial-scene-graph]
              (when (window/visible? window)

                (let [window-width (window/width window)
                      window-height (window/height window)
                      scene-graph (with-profiling profiling :handle-events
                                    (loop [events (read-events event-channel target-frame-rate)
                                           scene-graph scene-graph]
                                      
                                      (if-let [event (first events)]
                                        (do (handle-event scene-graph event)
                                            (let [scene-graph (create-scene-graph window-width
                                                                                  window-height)]
                                              (handle-new-scene-graph scene-graph)
                                              (recur (rest events)
                                                     scene-graph)))
                                        scene-graph)))]

                  (async/>!! renderable-scene-graph-channel
                             scene-graph)
                  
                  (value-registry/delete-unused-values! 10000)
                  (recur scene-graph))))))

        (println "exiting event handling loop")

        (catch Throwable e
          (.printStackTrace e *out*)
          (window/close window)
          (throw e))
        (finally
          (async/close! renderable-scene-graph-channel))))
    event-channel))


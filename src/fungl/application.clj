(ns fungl.application
  (:require [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            [flow-gl.gui.animation :as animation]
            [flow-gl.gui.keyboard :as keyboard]
            [flow-gl.gui.mouse :as mouse]
            [flow-gl.gui.stateful :as stateful]
            [flow-gl.gui.window :as window]
            [flow-gl.opengl.jogl.window :as jogl-window]
            [fungl.cache :as cache]
            [fungl.layout :as layout]
            [fungl.renderer :as renderer]
            [fungl.root-renderer :as root-renderer]
            [fungl.value-registry :as value-registry]
            [logga.core :as logga]))

(require 'taoensso.timbre.profiling)

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

(defn create-render-state []
  (conj (cache/state-bindings)
        (value-registry/state-bindings)))

(defn render [gl scene-graph]
  (renderer/apply-renderers! (assoc scene-graph
                                    :render (if-let [render (:render scene-graph)]
                                              render
                                              root-renderer/root-renderer))
                             gl)
  
  #_(let [{:keys [width height]} (opengl/size gl)]
      (quad-renderer/draw! (scene-graph/leave-nodes scene-graph)
                           width height
                           gl)))

(defn handle-event! [scene-graph event]
  (do ;;taoensso.timbre.profiling/profile :info :handle-event
    (when (= :mouse
             (:source event))
      (mouse/handle-mouse-event! event))

    (when (= :keyboard
             (:source event))
      (keyboard/handle-keyboard-event! scene-graph event))))

(defn handle-new-scene-graph! [scene-graph]
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

(defmacro thread [name & body]
  ;; use async/thread to inherit bindings such as flow-gl.debug/dynamic-debug-channel
  `(.start (Thread. (fn [] ~@body)
                    ~name)))

(defn run-create-schene-graph [create-scene-graph window-width window-height]
  (let [scene-graph (create-scene-graph window-width
                                        window-height)]
    (handle-new-scene-graph! scene-graph)
    scene-graph))

(defn start-event-thread [create-scene-graph event-channel renderable-scene-graph-channel initial-width initial-height target-frame-rate do-profile]
  (thread "event"
          (logga/write "starting event loop")
          (try
            (with-bindings (create-event-handling-state)
              (let [initial-scene-graph (create-scene-graph initial-width initial-height)]
                (handle-new-scene-graph! initial-scene-graph)
                (loop [scene-graph initial-scene-graph
                       window-width initial-width
                       window-height initial-height]
                  (let [[scene-graph window-width window-height] (with-profiling do-profile :handle-events
                                                                   (loop [events (read-events event-channel target-frame-rate)
                                                                          scene-graph scene-graph
                                                                          window-width window-width
                                                                          window-height window-height]
                                                                     (if-let [event (first events)]
                                                                       (do #_(logga/write "handling" event)
                                                                           (handle-event! scene-graph event)
                                                                           (cond (= :close-requested (:type event))
                                                                                 nil

                                                                                 (= :resize-requested (:type event))
                                                                                 (recur (rest events)
                                                                                        (run-create-schene-graph create-scene-graph
                                                                                                                 (:width event)
                                                                                                                 (:height event))
                                                                                        (:width event)
                                                                                        (:height event))

                                                                                 :default
                                                                                 (recur (rest events)
                                                                                        (run-create-schene-graph create-scene-graph
                                                                                                                 window-width
                                                                                                                 window-height)
                                                                                        window-width
                                                                                        window-height)))
                                                                       [scene-graph
                                                                        window-width
                                                                        window-height])))]

                    (when scene-graph

                      (async/>!! renderable-scene-graph-channel
                                 scene-graph)
                      (value-registry/delete-unused-values! 10000)
                      (recur scene-graph
                             window-width
                             window-height))))))

            (logga/write "exiting event handling loop")

            (catch Throwable e
              (logga/write (prn-str e))
              (throw e))
            (finally
              (async/close! renderable-scene-graph-channel)))))

(defn start-window [create-scene-graph & {:keys [window
                                                 target-frame-rate
                                                 handle-event!
                                                 do-profiling
                                                 ;;                                                  render
                                                 ;;                                                  create-event-handling-state
                                                 ;;                                                  create-render-state
                                                 ;;                                                  read-events
                                                 ]
                                          :or {target-frame-rate 60
                                               handle-event! handle-event!
                                               do-profiling false
                                               ;;                                                read-events read-events
                                               ;;                                                render render
                                               ;;                                                create-event-handling-state create-event-handling-state
                                               ;;                                                create-render-state create-render-state
                                               }}]
  (let [event-channel-promise (promise)]
    (thread "render"
            (logga/write "creating window")
            (let [window (or window
                             (create-window))
                  renderable-scene-graph-channel (async/chan)
                  render-state (create-render-state)]
              (deliver event-channel-promise (window/event-channel window))
              (start-event-thread create-scene-graph
                                  (window/event-channel window)
                                  renderable-scene-graph-channel
                                  (window/width window)
                                  (window/height window)
                                  target-frame-rate
                                  do-profiling)

              (try (logga/write "starting render loop")
                   (loop []
                     (when-let [scene-graph (async/<!! renderable-scene-graph-channel)]
                       (window/with-gl window gl
                         (with-profiling do-profiling :render
                           (with-bindings render-state
                             (render gl scene-graph)
                             (value-registry/delete-unused-values! 500))))
                       (window/swap-buffers window)
                       (recur)))

                   (logga/write "closing window")
                   (window/close window)
                   (logga/write "exiting render loop")

                   (catch Throwable e
                     (logga/write (prn-str e))
                     (window/close window)
                     (throw e)))))
    @event-channel-promise))

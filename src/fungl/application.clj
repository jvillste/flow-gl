(ns fungl.application
  (:require [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            [flow-gl.gui.animation :as animation]
            [flow-gl.gui.keyboard :as keyboard]
            [flow-gl.gui.mouse :as mouse]
            [flow-gl.gui.stateful :as stateful]
            [flow-gl.gui.window :as window]
            [flow-gl.swing.window :as swing-window]
            [fungl.cache :as cache]
            [fungl.layout :as layout]
            [fungl.renderer :as renderer]
            [fungl.swing.root-renderer :as swing-root-renderer]
            [fungl.value-registry :as value-registry]
            [fungl.view-compiler :as view-compiler]
            [logga.core :as logga]
            [taoensso.tufte :as tufte]))

(defn create-event-handling-state []
  (conj (stateful/state-bindings :delete-after-calls 50)
        (mouse/state-bindings)
        (keyboard/state-bindings)
        (animation/state-bindings)
        (cache/state-bindings)
        (value-registry/state-bindings)
        (view-compiler/state-bindings)))

(defn create-render-state []
  (conj (cache/state-bindings)
        (value-registry/state-bindings)))

(defn render [gl scene-graph]
  (renderer/apply-renderers! (assoc scene-graph
                                    :render (if-let [render (:render scene-graph)]
                                              render
                                              swing-root-renderer/root-renderer
                                              #_root-renderer/root-renderer))
                             gl))

(defn handle-event! [scene-graph event]
  (do ;;taoensso.tufte/profile :info :handle-event
    (when (= :mouse
             (:source event))
      (mouse/handle-mouse-event! event))

    (when (= :keyboard
             (:source event))
      (keyboard/handle-keyboard-event! scene-graph event))))

(defn handle-new-scene-graph! [scene-graph]
  (keyboard/handle-new-scene-graph! scene-graph)
  (mouse/handle-new-scene-graph! scene-graph))

(defn create-window
  ([]
   (create-window 400 400))
  ([width height]
   #_(jogl-window/create 400 400
                         :close-automatically true)
   (swing-window/create width height)))

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
     (taoensso.tufte/profile {:id ~id}
                             ~@body)
     (do ~@body)))

(defmacro thread [name & body]
  ;; use async/thread to inherit bindings such as flow-gl.debug/dynamic-debug-channel
  `(.start (Thread. (fn [] ~@body)
                    ~name)))

(defn run-create-scene-graph [create-scene-graph window-width window-height]
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
                  (let [[scene-graph window-width window-height] (loop [events (read-events event-channel target-frame-rate)
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
                                                                                      (run-create-scene-graph create-scene-graph
                                                                                                              (:width event)
                                                                                                              (:height event))
                                                                                      (:width event)
                                                                                      (:height event))

                                                                               :default
                                                                               (recur (rest events)
                                                                                      (run-create-scene-graph create-scene-graph
                                                                                                              window-width
                                                                                                              window-height)
                                                                                      window-width
                                                                                      window-height)))
                                                                     [scene-graph
                                                                      window-width
                                                                      window-height]))]

                    (when scene-graph

                      (async/>!! renderable-scene-graph-channel
                                 scene-graph)
                      (value-registry/delete-unused-values! 10000)
                      (recur scene-graph
                             window-width
                             window-height))))))

            (logga/write "exiting event handling loop")

            (catch Throwable e
              (logga/write "Exception in event loop" (prn-str e)))
            (finally
              (async/close! renderable-scene-graph-channel)))))

(defn start-window [root-view-or-var & {:keys [window
                                               target-frame-rate
                                               do-profiling
                                               on-exit]
                                        :or {target-frame-rate 60
                                             do-profiling false}}]
  (let [event-channel-promise (promise)]
    (thread "render"
            (logga/write "creating window")
            (let [window (or window
                             (create-window))
                  renderable-scene-graph-channel (async/chan)
                  render-state (create-render-state)]
              (deliver event-channel-promise (window/event-channel window))
              (start-event-thread (fn [width height]
                                    (-> (view-compiler/compile [(if (var? root-view-or-var)
                                                                  @root-view-or-var
                                                                  root-view-or-var)])
                                        (layout/do-layout-for-size width height)))
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
                   (logga/write "exiting render loop")

                   (when on-exit
                     (on-exit))
                   (catch Exception e
                     (logga/write "Exception in render loop:" (prn-str e))
                     (throw e))
                   (finally
                     (logga/write "closing window")
                     (window/close window)
                     ;; events must be read from the channel so that the swing event loop can handle the close event
                     (csp/drain @event-channel-promise 0)
                     ))))
    @event-channel-promise))

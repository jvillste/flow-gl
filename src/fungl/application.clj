(ns fungl.application
  (:require
   [clojure.core.async :as async]
   [flow-gl.csp :as csp]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.mouse :as mouse]
   [flow-gl.gui.stateful :as stateful]
   [flow-gl.gui.visuals :as visuals]
   [flow-gl.gui.window :as window]
   [flow-gl.swing.window :as swing-window]
   [fungl.cache :as cache]
   [fungl.layout :as layout]
   [fungl.renderer :as renderer]
   [fungl.swing.root-renderer :as swing-root-renderer]
   [fungl.value-registry :as value-registry]
   [fungl.view-compiler :as view-compiler]
   [logga.core :as logga]
   [taoensso.tufte :as tufte]
   [flow-gl.gui.scene-graph :as scene-graph]
   [fungl.component :as component]
   [clojure.walk :as walk]
   [flow-gl.tools.trace :as trace]
   [fungl.depend :as depend]
   [clojure.test :refer [deftest is]]))

(tufte/add-basic-println-handler! {})


(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom {:highlight-view-call-cache-misses? true #_false
                       :window-width nil
                       :window-height nil})})

(defn create-event-handling-state []
  (conj (state-bindings)
        (stateful/state-bindings :delete-after-calls 50)
        (mouse/state-bindings)
        (keyboard/state-bindings)
        (animation/state-bindings)
        (cache/state-bindings)
        (view-compiler/state-bindings)))

(defn create-render-state []
  (conj (cache/state-bindings)
        (value-registry/state-bindings)
        (visuals/state-bindings)))

(defn render [gl scene-graph]
  (renderer/apply-renderers! (assoc scene-graph
                                    :render
                                    swing-root-renderer/render-scene-graph)
                             gl))

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

(defn describe-dependables []
  (println)
  (println "dependables:")

  (run! (fn [dependency]
          (println (name dependency)
                   (hash dependency)
                   (view-compiler/describe-value (depend/current-value dependency))))
        (cache/all-dependencies))

  (println))

(defn run-create-scene-graph [create-scene-graph]
  (let [scene-graph (taoensso.tufte/p :create-scene-graph
                                      (create-scene-graph (:window-width @state-atom)
                                                          (:window-height @state-atom)))]

    (describe-dependables)
    (println "component tree:")
    (view-compiler/print-component-tree (view-compiler/component-tree scene-graph))
    (println)
    (handle-new-scene-graph! scene-graph)
    scene-graph))

(def ^:dynamic event-channel)

(defn send-event! [event]
  (async/put! event-channel
              event))

(defn cache-miss? [node]
  (and (contains? node :view-functions)
       (some view-compiler/changed-dependency?
             (mapcat :dependencies
                     (:view-functions node)))))

(deftest test-cache-miss?
  (is (cache-miss? {:view-functions [{:dependencies ()}
                                     {:dependencies [{:new-value 1, :old-value 2}]}]})))


(defn highlight-cache-misses [scene-graph]
  (if (:highlight-view-call-cache-misses? @state-atom)
    (scene-graph/update-depth-first scene-graph
                                    cache-miss?
                                    (fn [node]
                                      (component/color-overlay [255 0 0 30]
                                                               node)))
    scene-graph))


(comment
  (do
    (trace/trace-var 'flow-gl.gui.scene-graph/flatten)
    (trace/trace-ns 'fungl.application)
    (trace/trace-ns 'fungl.view-compiler)
    (trace/trace-ns 'fungl.layout)
;;    (trace/trace-ns 'argupedia.ui2)
    )
  ) ;; TODO: remove-me

(def layout-trace-value-atom (atom {}))

;; layout
;; view compilation
;; component texture cache
;; how to test scene graph generation and event handlig without rendering

(defn process-event [create-scene-graph scene-graph event]
  (println)
  (println "handling" (:type event) (:key event))

  (when (= :mouse
           (:source event))
    (mouse/handle-mouse-event! event))

  (when (= :keyboard
           (:source event))
    (keyboard/handle-keyboard-event! scene-graph event))

  (if (= :close-requested (:type event))
    nil
    (do (when (= :redraw (:type event))
          (cache/invalidate-all!)
          (reset! (:constructor-cache view-compiler/state)
                  {}))

        (when (= :resize-requested (:type event))
          (swap! state-atom
                 assoc
                 :window-width (:width event)
                 :window-height (:height event)))

        (run-create-scene-graph create-scene-graph))))

(defn start-event-thread [create-scene-graph given-event-channel renderable-scene-graph-channel initial-width initial-height target-frame-rate do-profile]
  (thread "event"
          (logga/write "starting event loop")
          (try
            (with-profiling do-profile :event-thread
              (with-bindings (merge (create-event-handling-state)
                                    {#'event-channel given-event-channel})
                (swap! state-atom
                       assoc
                       :window-width initial-width
                       :window-height initial-height)
                (loop [scene-graph (run-create-scene-graph create-scene-graph)]


                  (let [scene-graph (loop [events (read-events given-event-channel target-frame-rate)
                                           scene-graph scene-graph]
                                      (if (empty? events)
                                        scene-graph
                                        (recur (rest events)
                                               (process-event create-scene-graph
                                                              scene-graph
                                                              (first events)))))]


                    (when scene-graph
                      (async/>!! renderable-scene-graph-channel
                                 (-> scene-graph
                                     (highlight-cache-misses)
                                     (layout/do-layout-for-size (:window-width @state-atom)
                                                                (:window-height @state-atom))))
                      (recur scene-graph))))))

            (logga/write "exiting event handling loop")

            (catch Throwable e
              (logga/write "Exception in event loop" (prn-str e)))
            (finally
              (async/close! renderable-scene-graph-channel)))))

(defn make-cached-components-render-to-images [scene-graph]
  (scene-graph/map-nodes (fn [node]
                           (if (and (map? node)
                                    (:view-functions node)
                                    (not (every? view-compiler/changed-dependency?
                                                 (-> node :view-functions :dependencies))))
                             (assoc node
                                    :render
                                    (or (:render node)
                                        visuals/render-to-images-render-function))
                             node))
                         scene-graph))

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
                                    (trace/with-call-tree-printing #_layout-trace-value-atom (atom {})
                                      (-> (view-compiler/compile-view-calls [root-view-or-var])
                                          (make-cached-components-render-to-images)
                                          (layout/do-layout-for-size width height))))
                                  (window/event-channel window)
                                  renderable-scene-graph-channel
                                  (window/width window)
                                  (window/height window)
                                  target-frame-rate
                                  do-profiling)

              (try (logga/write "starting render loop")
                   (with-profiling do-profiling :render-loop
                     (loop []
                       (when-let [scene-graph (async/<!! renderable-scene-graph-channel)]
                         (tufte/p :render
                                  (window/with-gl window gl
                                    (with-bindings render-state
                                      (render gl scene-graph)
                                      (value-registry/delete-unused-values! 500))))
                         (window/swap-buffers window)
                         (recur))))
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

;; (defn foo []
;;   (scene-graph/flatten {:x 1 :y 1 :width 1 :height 10})
;;   (scene-graph/flatten {:x 1 :y nil :width 1 :height nil}))

;; (trace/with-call-tree-printing (atom {})
;;   (foo))

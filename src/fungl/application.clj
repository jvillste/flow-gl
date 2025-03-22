(ns fungl.application
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer [deftest is]]
   [flow-gl.csp :as csp]
   [flow-gl.gui.animation :as animation]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.mouse :as mouse]
   [flow-gl.gui.scene-graph :as scene-graph]
   [flow-gl.gui.stateful :as stateful]
   [flow-gl.gui.visuals :as visuals]
   [flow-gl.gui.window :as window]
   [flow-gl.swing.window :as swing-window]
   [flow-gl.tools.trace :as trace]
   [fungl.cache :as cache]
   [fungl.component :as component]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
   [fungl.layout :as layout]
   [fungl.node-image-cache :as node-image-cache]
   [fungl.renderer :as renderer]
   [fungl.swing.root-renderer :as swing-root-renderer]
   [fungl.view-compiler :as view-compiler]
   [logga.core :as logga]
   [taoensso.tufte :as tufte]
   [fungl.identity-cache :as identity-cache]))

(tufte/add-basic-println-handler! {})


(def ^:dynamic application-loop-state-atom)
(def ^:dynamic apply-layout-nodes-cache-atom)

(def ^:dynamic state-atom) ;; applicaiton specific state

(defn state-bindings []
  {#'state-atom (dependable-atom/atom {})
   #'application-loop-state-atom (atom {:highlight-view-call-cache-misses? true #_false
                                        :window-width nil
                                        :window-height nil})
   #'apply-layout-nodes-cache-atom (identity-cache/create-cache-atom)})

(defn create-event-handling-state []
  (conj (state-bindings)
        (stateful/state-bindings :delete-after-calls 50)
        (mouse/state-bindings)
        (keyboard/state-bindings)
        (animation/state-bindings)
        (cache/state-bindings)
        (view-compiler/state-bindings)
        (layout/state-bindings)
        (scene-graph/state-bindings)))

(defn create-render-state []
  (node-image-cache/state-bindings))

(defn handle-new-scene-graph! [scene-graph]
  ;; (println "new scene graph:" (System/identityHashCode scene-graph))
  ;; (scene-graph/print-scene-graph (scene-graph/select-node-keys [:compilation-path :type :can-gain-focus? :keyboard-event-handler] scene-graph))
  (reset! scene-graph/current-scene-graph-atom scene-graph)
  (keyboard/handle-new-scene-graph! scene-graph)
  (mouse/handle-new-scene-graph! scene-graph))

(defn create-swing-window
  ([]
   (create-swing-window 400 400))
  ([width height]
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

(defn create-scene-graph [root-view-call]
  (let [scene-graph (layout/layout-scene-graph (view-compiler/compile-view-calls root-view-call)
                                               (:window-width @application-loop-state-atom)
                                               (:window-height @application-loop-state-atom))]

    (handle-new-scene-graph! (identity-cache/call-with-cache apply-layout-nodes-cache-atom
                                                             1
                                                             layout/apply-layout-nodes
                                                             scene-graph))
    scene-graph))

(defn send-event! [event]
  (async/put! (window/event-channel (:window @application-loop-state-atom))
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
  (if (:highlight-view-call-cache-misses? @application-loop-state-atom)
    (scene-graph/update-depth-first scene-graph
                                    cache-miss?
                                    (fn [node]
                                      (component/color-overlay [255 0 0 30]
                                                               node)))
    scene-graph))


(comment
  (do
    (trace/trace-var 'flow-gl.gui.scene-graph/flatten)
    (trace/trace-var 'fungl.layout/do-layout)
    (trace/trace-ns 'fungl.application)
    (trace/untrace-ns 'fungl.view-compiler)
    (trace/untrace-ns 'fungl.layout)
    (trace/trace-ns 'fungl.layout)

    (trace/untrace-var 'swing-root-renderer/render-scene-graph)
    ;;    (trace/trace-ns 'argupedia.ui2)
    )
  )


(defonce events-atom (atom [])) ;; TODO: remove me
(comment
  (first @events-atom)
  (distinct (map :type @events-atom))
  (count @events-atom)
  (take 20 @events-atom)
  (reset! events-atom [])
  )

(defn process-event! [event]
  ;; (swap! events-atom conj event)

  ;; (prn "cache stats" (cache/stats cache/state)) ;; TODO: remove me

  ;; (println)
  ;; (println "handling" (:type event) (:key event) #_(pr-str event))
  ;; (println)

  (when (= :mouse
           (:source event))
    (mouse/handle-mouse-event! event))

  (when (= :keyboard
           (:source event))
    (keyboard/handle-keyboard-event! event))

  (when (or (= :redraw (:type event))
            (keyboard/key-pattern-pressed? [#{:alt :meta} :r]
                                           event))
    (cache/invalidate-all!)
    (reset! layout/layout-node-cache-atom (hierarchical-identity-cache/initial-state))
    (reset! layout/adapt-to-space-cache-atom (hierarchical-identity-cache/initial-state)))

  (when (= :resize-requested (:type event))
    (swap! application-loop-state-atom
           assoc
           :window-width (:width event)
           :window-height (:height event)))

  (when (keyboard/key-pattern-pressed? [#{:alt :meta} :i]
                                       event)
    (swap! application-loop-state-atom update :show-development-tools? not))

  ;;  (Thread/sleep 500)
  )

(defn close-window! [window]
  (window/close window)
  ;; events must be read from the channel so that the swing event loop
  ;; can handle the close event
  (csp/drain (window/event-channel window)
             0))

(defn close! [state]
  (close-window! (:window state)))

(defn create-bindings-without-window [root-view-call]
  (let [bindings (merge (create-event-handling-state)
                        (create-render-state))]
    (with-bindings bindings
      (swap! application-loop-state-atom
             assoc
             :window-width 1000
             :window-height 1000
             :root-view-call root-view-call)
      (let [scene-graph (create-scene-graph root-view-call)]
        (swap! application-loop-state-atom
               assoc
               :scene-graph scene-graph)))
    bindings))

(defn preserve-only-latest-event [filtered-event-type events]
  (reverse (loop [events (reverse events)
                  filtered-event-encountered? false
                  filtered-events []]
             (if-let [event (first events)]
               (recur (rest events)
                      (or filtered-event-encountered?
                          (= filtered-event-type
                             (:type event)))
                      (if (or (not filtered-event-encountered?)
                              (not (= filtered-event-type
                                      (:type event))))
                        (conj filtered-events event)
                        filtered-events))
               filtered-events))))

(deftest test-preserve-only-latest-event
  (is (= '({:number 1, :type :a}
           {:number 3, :type :c}
           {:number 4, :type :b})
         (preserve-only-latest-event :b
                                     [{:number 1 :type :a}
                                      {:number 2 :type :b}
                                      {:number 3 :type :c}
                                      {:number 4 :type :b}]))))

(defn handle-events! [events]
  ;; (logga/write 'handle-events (map :type events))
  ;; (logga/write 'events events) ;; TODO: remove me

  (let [new-scene-graph (loop [events (preserve-only-latest-event :mouse-moved events)
                               scene-graph (:scene-graph @application-loop-state-atom)]
                          (if (empty? events)
                            scene-graph
                            (if (or (= :close-requested (:type (first events)))
                                    (keyboard/key-pattern-pressed? [#{:alt :meta} :w]
                                                                   (first events)))
                              nil
                              (do (process-event! (first events))
                                  (recur (rest events)
                                         (create-scene-graph (:root-view-call @application-loop-state-atom)))))))]
    (swap! application-loop-state-atom
           assoc
           :scene-graph
           new-scene-graph)))

(defmacro thread [name & body]
  `(.start (Thread. (bound-fn [] ~@body)
                    ~name)))

(comment
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-ui 9898)
  (prof/profile (doseq [i (range 1000000)]
                  (* i i)))
  (set! *warn-on-reflection* true)
  (set! *warn-on-reflection* false)

  (prof/profile {:interval (let [millisecond 1000000
                                 framerate 100]
                             (/ (* 1000 millisecond)
                                framerate))}
                (doseq [event (take 1 @application/events-atom)]
                  (async/>!! event-channel
                             event)))
  )

(defn with-window-gl [body]
  (window/with-gl (:window @application-loop-state-atom) gl
    (body gl))
  (window/swap-buffers (:window @application-loop-state-atom)))

(defn render! [nodes-to-image-node with-gl render-nodes]
  (when-some [scene-graph (:scene-graph @application-loop-state-atom)]

    (let [previous-rendered-scene-graph (:previous-rendered-scene-graph @application-loop-state-atom)]
      (when (not (identical? scene-graph
                             previous-rendered-scene-graph))

        (with-gl (fn [gl]
                   (let [scene-graph (->> scene-graph
                                          (node-image-cache/render-recurring-nodes-to-images nodes-to-image-node previous-rendered-scene-graph)
                                          ;; (renderer/apply-renderers! gl) ;; TODO: this should be cached to make the scene graph stay identical when possible
                                          (identity-cache/call-with-cache apply-layout-nodes-cache-atom
                                                                          1
                                                                          layout/apply-layout-nodes))]
                     (->> (scene-graph/scene-graph-nodes-in-view scene-graph
                                                                 (:width scene-graph)
                                                                 (:height scene-graph))
                          (filter :draw-function)
                          (render-nodes gl)))))

        (swap! application-loop-state-atom assoc :previous-rendered-scene-graph scene-graph)))))

(defn render-to-swing-window! []
  (render! visuals/nodes-to-buffered-image-node
           with-window-gl
           (fn [graphics nodes]
             (swing-root-renderer/render-nodes graphics
                                               nodes
                                               {:color-nodes? false}))))

(defn create-bindings-wight-swing-window [root-view-call]
  (let [bindings (create-bindings-without-window root-view-call)]
    (with-bindings bindings
      (swap! application-loop-state-atom
             assoc
             :window (create-swing-window (:window-width @application-loop-state-atom)
                                          (:window-height @application-loop-state-atom))))
    bindings))

(defn start-application [root-view & {:keys [target-frame-rate
                                             on-exit]
                                      :or {target-frame-rate 60}}]
  (println "------------ start-window -------------")
  (with-bindings (create-bindings-wight-swing-window [root-view])
    (handle-events! [{:type :resize-requested
                      :width (* 2 (:window-width @application-loop-state-atom)),
                      :height (* 2 (:window-height @application-loop-state-atom))}])

    (render-to-swing-window!)

    (thread "fungl application"
            (try (loop []
                   (identity-cache/with-cache-cleanup apply-layout-nodes-cache-atom
                     (handle-events! (read-events (window/event-channel (:window @application-loop-state-atom))
                                                  target-frame-rate))
                     (render-to-swing-window!))

                   (when (:scene-graph @application-loop-state-atom)
                     (recur)))

                 (logga/write "exiting application loop")

                 (catch Exception e
                   (logga/write "Exception in application loop:" (prn-str e))
                   (throw e))
                 (finally
                   (when on-exit
                     (on-exit))
                   (logga/write "closing window")
                   (close-window! (:window @application-loop-state-atom)))))
    (window/event-channel (:window @application-loop-state-atom))))

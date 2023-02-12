(ns fungl.application
  (:require
   [flow-gl.debug :as debug]
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
   [clojure.test :refer [deftest is]]
   [clojure.string :as string]
   [fungl.id-comparator :as id-comparator]
   [clojure.set :as set]
   [fungl.util :as util]))

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
  (conj #_(cache/state-bindings)
        #_(value-registry/state-bindings)
        (visuals/state-bindings)))

;; render gets graphics context and returns scene graph
;; draw-function gets graphics context and returns nil

;; application/render
;;   renderer/apply-renderers!
;;     swing-root-renderer/render-scene-graph
;;     visuals/render-to-images-render-function
;;       visuals/render-to-images    takes leaf nodes, groups them by z and renders an image for each z value
;;         swing-root-renderer/render-to-buffered-image    render given nodes to an image
;;           swing-root-renderer/render-nodes    calls given nodes draw-functions

(defn render [gl scene-graph]
  (when (not (= scene-graph
                (:previous-rendered-scene-graph @state-atom)))
    (swap! state-atom assoc :previous-rendered-scene-graph scene-graph)

    (swing-root-renderer/render-scene-graph gl
                                            (renderer/apply-renderers! scene-graph
                                                                       gl))))

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

(defn describe-dependables []
  (println)
  (println "dependables:")
  (run! (fn [dependable]
          (println (name dependable)
                   (hash dependable)
                   (view-compiler/describe-value (depend/current-value dependable))))
        (cache/all-dependables))

  (println))

(defonce layout-trace-value-atom (atom {}))

(defn print-component-tree [old-view-call-dependency-value-maps]
  (doseq [view-call-id (->> (keys (:view-functions @view-compiler/state))
                            (sort-by identity id-comparator/compare-ids))]

    (println view-call-id
             (when-let [view-function (get (:view-functions @view-compiler/state)
                                           view-call-id)]
               (view-compiler/function-name (if (var? view-function)
                                              @view-function
                                              view-function)))
             (->> (get (:node-dependencies @view-compiler/state)
                       view-call-id)
                  (map (fn [[dependable _value]]
                         (let [old-value (get (get old-view-call-dependency-value-maps
                                                   view-call-id)
                                              dependable)
                               current-value (depend/current-value dependable)
                               changed? (not (= old-value
                                                current-value))]
                           (str (when changed?
                                  (util/escapes :red))
                                (name dependable)
                                #_(if changed?
                                    "!"
                                    "")
                                " "
                                (if changed?
                                  (str (view-compiler/describe-value old-value)
                                       " -> "
                                       (view-compiler/describe-value current-value))
                                  (view-compiler/describe-value current-value))
                                (when changed?
                                  (util/escapes :reset))))))
                  (sort)))))

(defn create-scene-graph [root-view-or-var]
  (let [view-call-dependency-value-maps-before-compilation (:node-dependencies @view-compiler/state)]
    (view-compiler/start-compilation-cycle!)
    (let [scene-graph (layout/do-layout-for-size (view-compiler/compile-view-calls [root-view-or-var])
                                                 (:window-width @state-atom)
                                                 (:window-height @state-atom))]
      (view-compiler/end-compilation-cycle!)
      ;; (describe-dependables)

      (println)
      (print-component-tree view-call-dependency-value-maps-before-compilation)
      (println)

      ;; (println "node dependencies")
      ;; (doseq [[node-id dependables] (sort-by first
      ;;                                        id-comparator/compare-ids
      ;;                                        (seq @(:node-dependencies view-compiler/state)))]
      ;;   (println node-id (sort (map name (keys dependables)))))

      (handle-new-scene-graph! scene-graph)
      scene-graph)))

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
    (trace/trace-var 'fungl.layout/do-layout)
    (trace/trace-ns 'fungl.application)
    (trace/untrace-ns 'fungl.view-compiler)
    (trace/untrace-ns 'fungl.layout)
    (trace/trace-ns 'fungl.layout)

    (trace/untrace-var 'swing-root-renderer/render-scene-graph)
    ;;    (trace/trace-ns 'argupedia.ui2)
    )
  )



(comment
  (count @layout-trace-value-atom)
  (get @layout-trace-value-atom
       580955494)
  ) ;; TODO: remove me


(defonce events-atom (atom [])) ;; TODO: remove me
(comment
  (first @events-atom)
  ) ;; TODO: remove me

(defn process-event! [scene-graph event]
  (swap! events-atom conj event) ;; TODO: remove me

  (println)
  (println "handling" (:type event) (:key event) #_(pr-str event))
  (println)

  (when (= :mouse
           (:source event))
    (mouse/handle-mouse-event! event))

  (when (= :keyboard
           (:source event))
    (keyboard/handle-keyboard-event! scene-graph event))

  (when (= :redraw (:type event))
    (cache/invalidate-all!)
    (swap!  view-compiler/state
            assoc
            :constructor-cache
            {}))

  (when (= :resize-requested (:type event))
    (swap! state-atom
           assoc
           :window-width (:width event)
           :window-height (:height event))))

(defn close-window! [window]
  (window/close window)
  ;; events must be read from the channel so that the swing event loop
  ;; can handle the close event
  (csp/drain (window/event-channel window)
             0))

(defn close! [state]
  (close-window! (:window state)))

(defn create-bindings-without-window [root-view]
  (let [bindings (merge (create-event-handling-state)
                        (create-render-state))]
    (with-bindings bindings
      (swap! state-atom
             assoc
             :window-width 400
             :window-height 400
             :root-view root-view)
      (let [scene-graph (create-scene-graph root-view)]
        (swap! state-atom
               assoc
               :scene-graph scene-graph)))
    bindings))

(defn create-bindings [root-view]
  (let [bindings (create-bindings-without-window root-view)]
    (with-bindings bindings
      (swap! state-atom
             assoc
             :window (create-window (:window-width @state-atom)
                                    (:window-height @state-atom))))
    bindings))

(defn handle-events! [events]
  (swap! state-atom
         assoc
         :scene-graph
         (loop [events events
                scene-graph (:scene-graph @state-atom)]
           (if (empty? events)
             scene-graph
             (if (= :close-requested (:type (first events)))
               nil
               (do (process-event! scene-graph
                                   (first events))
                   (recur (rest events)
                          (create-scene-graph (:root-view @state-atom)))))))))

(defmacro thread [name & body]
  `(.start (Thread. (bound-fn [] ~@body)
                    ~name)))

(defn start-application [root-view & {:keys [target-frame-rate
                                             on-exit]
                                      :or {target-frame-rate 60}}]
  (println "------------ start-window -------------")
  (with-bindings (create-bindings root-view)
    (thread "fungl application"
            (try (loop []
                   (handle-events! (read-events (window/event-channel (:window @state-atom))
                                                target-frame-rate))

                   (when (:scene-graph @state-atom)
                     (window/with-gl (:window @state-atom) gl
                       (render gl (:scene-graph @state-atom)))
                     (window/swap-buffers (:window @state-atom))
                     (recur)))

                 (logga/write "exiting application loop")

                 (catch Exception e
                   (logga/write "Exception in application loop:" (prn-str e))
                   (throw e))
                 (finally
                   (when on-exit
                     (on-exit))
                   (logga/write "closing window")
                   (close-window! (:window @state-atom)))))
    (window/event-channel (:window @state-atom))))

#_(defn start-window [root-view-or-var & {:keys [window
                                               target-frame-rate
                                               do-profiling
                                               on-exit]
                                        :or {target-frame-rate 60
                                             do-profiling false}}]
  (println "------------ start-window -------------")
  (reset! events-atom []) ;; TODO: remove me

  (let [event-channel-promise (promise)]
    (thread "application"
            (logga/write "creating window")
            (let [window (or window
                             (create-window))]
              (deliver event-channel-promise (window/event-channel window))

              (try (logga/write "starting application loop")
                   (with-profiling do-profiling :render-loop
                     (with-bindings (merge (create-event-handling-state)
                                           {#'event-channel (window/event-channel (create-window))}
                                           (create-render-state))
                       (swap! state-atom
                              assoc
                              :window-width (window/width window)
                              :window-height (window/height window))
                       (loop [scene-graph (create-scene-graph root-view-or-var)]
                         (let [scene-graph (loop [events (read-events (window/event-channel window)
                                                                      target-frame-rate)
                                                  scene-graph scene-graph]
                                             (if (empty? events)
                                               scene-graph
                                               (if (= :close-requested (:type (first events)))
                                                 nil
                                                 (do (process-event! scene-graph
                                                                     (first events))
                                                     (recur (rest events)
                                                            (create-scene-graph root-view-or-var))))))]
                           (when scene-graph
                             (let [scene-graph (-> scene-graph
                                                   #_(highlight-cache-misses)
                                                   #_(layout/do-layout-for-size (:window-width @state-atom)
                                                                                (:window-height @state-atom)))]

                               (tufte/p :render
                                        (window/with-gl window gl
                                          (render gl scene-graph)
                                          (value-registry/delete-unused-values! 500)))
                               (window/swap-buffers window)
                               (recur scene-graph)))))))
                   (logga/write "exiting application loop")

                   (when on-exit
                     (on-exit))
                   (catch Exception e
                     (logga/write "Exception in application loop:" (prn-str e))
                     (throw e))
                   (finally
                     (logga/write "closing window")
                     (window/close window)
                     ;; events must be read from the channel so that the swing event loop can handle the close event
                     (csp/drain @event-channel-promise 0)))))
    @event-channel-promise))


;; TODO: commit the refactoring of the application loop
;; then create a test bench to application-test with a test "window" that inly records rendering commands in a log that can be checked in tests

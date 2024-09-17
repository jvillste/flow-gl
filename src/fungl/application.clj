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
   [flow-gl.gui.window :as window]
   [flow-gl.swing.window :as swing-window]
   [flow-gl.tools.trace :as trace]
   [fungl.cache :as cache]
   [fungl.component :as component]
   [fungl.depend :as depend]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.id-comparator :as id-comparator]
   [fungl.layout :as layout]
   [fungl.node-image-cache :as node-image-cache]
   [fungl.renderer :as renderer]
   [fungl.swing.root-renderer :as swing-root-renderer]
   [fungl.util :as util]
   [fungl.view-compiler :as view-compiler]
   [logga.core :as logga]
   [taoensso.tufte :as tufte]
   [fungl.layouts :as layouts]
   [fungl.component.text-area :as text-area]

   [flow-gl.graphics.font :as font]
   [flow-gl.gui.visuals :as visuals]
   [clojure.string :as string]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(tufte/add-basic-println-handler! {})


(def ^:dynamic application-loop-state-atom)
(def ^:dynamic state-atom) ;; applicaiton specific state

(defn state-bindings []
  {#'state-atom (dependable-atom/atom {})
   #'application-loop-state-atom (atom {:highlight-view-call-cache-misses? true #_false
                                        :window-width nil
                                        :window-height nil})})

(defn create-event-handling-state []
  (conj (state-bindings)
        (stateful/state-bindings :delete-after-calls 50)
        (mouse/state-bindings)
        (keyboard/state-bindings)
        (animation/state-bindings)
        (cache/state-bindings)
        (view-compiler/state-bindings)
        (layout/state-bindings)))

(defn create-render-state []
  (conj #_(cache/state-bindings)
        #_(value-registry/state-bindings)
        (node-image-cache/state-bindings)))

;; render gets graphics context and returns scene graph
;; draw-function gets graphics context and returns nil

;; application/render
;;   renderer/apply-renderers!
;;     swing-root-renderer/render-scene-graph
;;     visuals/render-to-images-render-function
;;       visuals/render-to-images    takes leaf nodes, groups them by z and renders an image for each z value
;;         swing-root-renderer/render-to-buffered-image    render given nodes to an image
;;           swing-root-renderer/render-nodes    calls given nodes draw-functions

;; TODO: investigate differences in deeper paths
(defn node [scene-graph path]
  (scene-graph/get-in-path scene-graph
                           (scene-graph/id-to-local-id-path path)))

(defn node-without-children [scene-graph path]
  (-> (node scene-graph (scene-graph/id-to-local-id-path path))
      (dissoc :children)))

(comment
  (def path [1 0])

  (do (def the-saved-scene-graph the-scene-graph)
      (def the-saved-previous-scene-graph the-previous-scene-graph))

  (clojure.data/diff the-saved-scene-graph
                     the-saved-previous-scene-graph)


  (take 2 (clojure.data/diff (node the-saved-scene-graph path)
                             (node the-saved-previous-scene-graph path)))

  (clojure.data/diff the-saved-scene-graph
                     the-saved-previous-scene-graph)

  (:type (node the-saved-scene-graph path))

  (= (node the-saved-scene-graph path)
     (node the-saved-previous-scene-graph path))

  (scene-graph/print-scene-graph (scene-graph/select-node-keys [:type :id]
                                                               (node the-saved-scene-graph
                                                                     path)))
  )


(def font (font/create-by-name "CourierNewPSMT" 40))
(def bold-font (font/create-by-name "CourierNewPS-BoldMT" 40))

(defn text [string & [{:keys [font color] :or {font font
                                               color [255 255 255 255]}}]]
  (text-area/text (str string)
                  color
                  font))

(defn fully-qualified-function-or-var-name [function-or-var]
  (if (var? function-or-var)
    (str (:ns (meta function-or-var))
         "/"
         (:name (meta function-or-var)))
    (let [function-string (pr-str function-or-var)]
      (subs function-string
            10
            (dec (count function-string))))))

(deftest test-fully-qualified-function-or-var-name
  (is (= "fungl.application/fully-qualified-function-or-var-name"
         (fully-qualified-function-or-var-name fully-qualified-function-or-var-name)))

  (is (= "fungl.application/fully-qualified-function-or-var-name"
         (fully-qualified-function-or-var-name #'fully-qualified-function-or-var-name))))

(defn unqualified-function-or-var-name [function-or-var]
  (when function-or-var
    (if (var? function-or-var)
      (str (:name (meta function-or-var)))
      (let [function-string (pr-str function-or-var)]
        (second (string/split (subs function-string
                                    10
                                    (dec (count function-string)))
                              #"/"))))))

(deftest test-uqualified-function-or-var-name
  (is (= "unqualified-function-or-var-name"
         (unqualified-function-or-var-name unqualified-function-or-var-name)))

  (is (= "unqualified-function-or-var-name"
         (unqualified-function-or-var-name #'unqualified-function-or-var-name))))

(defn property [label value]
  (layouts/horizontally-2 {:margin 20}
                          (text label
                                {:font bold-font
                                 :color [100 200 100 255]})
                          (text value)))

(defn development-tools-view [scene-graph]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color [50 50 50 220])
               (layouts/vertically-2 {:margin 20}
                                     (property "view functions:"
                                               (string/join " " (map unqualified-function-or-var-name (remove nil? (map :view-function (scene-graph/path-to scene-graph
                                                                                                                                                            (:focused-node-id @keyboard/state-atom)))))))
                                     (property "command sets:"
                                               (string/join " " (map pr-str (remove nil? (map :name (map :command-set (scene-graph/path-to scene-graph
                                                                                                                                           (:focused-node-id @keyboard/state-atom))))))))
                                     (property "focused node id:"
                                               (:focused-node-id @keyboard/state-atom))

                                     (property "focused keyboard event handler:"
                                               (unqualified-function-or-var-name (first (:keyboard-event-handler (:focused-node @keyboard/state-atom))))))))

(defn add-development-tools [scene-graph]
  (if (:show-development-tools? @application-loop-state-atom)
    (let [development-tools-layout (layout/layout-scene-graph (assoc (view-compiler/compile-view-calls (development-tools-view scene-graph))
                                                                     :x 0
                                                                     :y 0)
                                                              (:window-width @application-loop-state-atom)
                                                              (:window-height @application-loop-state-atom))]
      {:children [scene-graph
                  (assoc development-tools-layout
                         :y (- (:window-height @application-loop-state-atom)
                               (:height development-tools-layout)))]
       :x 0
       :y 0
       :width (:window-width @application-loop-state-atom)
       :height (:window-height @application-loop-state-atom)})
    scene-graph))


;; (def render-count-atom (atom 0))
(defn render [scene-graph render-scene-graph gl]
  ;;  (println "render" (swap! render-count-atom inc)) ;; TODO: remove me
  (let [scene-graph (add-development-tools scene-graph)]
    (when (not (= scene-graph
                  (:previous-rendered-scene-graph @application-loop-state-atom)))

      ;; (def the-scene-graph scene-graph)
      ;; (def the-previous-scene-graph (:previous-rendered-scene-graph @application-loop-state-atom))

      ;; (let [path (scene-graph/id-to-local-id-path [1 :scrolling-pane 0 1 0 :notebook-body 0 [:value 0] :call])]
      ;;   (when (not (= (node scene-graph path)
      ;;                 (node (:previous-rendered-scene-graph @application-loop-state-atom) path)))
      ;;     (println "saving scene graphs")

      ;;     (def scene-graph scene-graph)
      ;;     (def previous-scene-graph (:previous-rendered-scene-graph @application-loop-state-atom))))

      (taoensso.tufte/p :render-scene-graph
                        (render-scene-graph gl
                                            (renderer/apply-renderers! (node-image-cache/render-recurring-nodes-to-images (:previous-rendered-scene-graph @application-loop-state-atom)
                                                                                                                          scene-graph)
                                                                       gl)))
      (swap! application-loop-state-atom assoc :previous-rendered-scene-graph scene-graph))))

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
    (let [scene-graph (layout/layout-scene-graph (view-compiler/compile-view-calls [root-view-or-var])
                                                 (:window-width @application-loop-state-atom)
                                                 (:window-height @application-loop-state-atom))]
      (view-compiler/end-compilation-cycle!)
      ;; (describe-dependables)

      ;; (println)
      ;; (print-component-tree view-call-dependency-value-maps-before-compilation)
      ;; (println)

      ;; (println "node dependencies")
      ;; (doseq [[node-id dependables] (sort-by first
      ;;                                        id-comparator/compare-ids
      ;;                                        (seq (:node-dependencies @view-compiler/state)))]
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



(comment
  (count @layout-trace-value-atom)
  (get @layout-trace-value-atom
       580955494)
  ) ;; TODO: remove me


(defonce events-atom (atom [])) ;; TODO: remove me
(comment
  (first @events-atom)
  (distinct (map :type @events-atom))
  (count @events-atom)
  (take 20 @events-atom)
  )

(defn process-event! [scene-graph-before-event-handling event]
  ;; (swap! events-atom conj event)

  ;; (println)
  ;; (println "handling" (:type event) (:key event) #_(pr-str event))
  ;; (println)

  (binding [scene-graph/current-scene-graph scene-graph-before-event-handling]
    (when (= :mouse
             (:source event))
      (mouse/handle-mouse-event! event))

    (when (= :keyboard
             (:source event))
      (keyboard/handle-keyboard-event! scene-graph-before-event-handling
                                       event)))

  (when (or (= :redraw (:type event))
            (keyboard/key-pattern-pressed? [#{:alt :meta} :r]
                                           event))
    (cache/invalidate-all!)
    (reset! layout/layout-node-cache-atom (hierarchical-identity-cache/initial-state))
    (reset! layout/adapt-to-space-cache-atom (hierarchical-identity-cache/initial-state))
    (swap!  view-compiler/state
            assoc
            :constructor-cache
            {})

    (swap!  view-compiler/state
            assoc
            :scene-graph-cache
            {}))

  (when (= :resize-requested (:type event))
    (swap! application-loop-state-atom
           assoc
           :window-width (:width event)
           :window-height (:height event)))

  (when (keyboard/key-pattern-pressed? [#{:alt :meta} :i]
                                       event)
    (swap! application-loop-state-atom update :show-development-tools? not)))

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
      (swap! application-loop-state-atom
             assoc
             :window-width 400
             :window-height 400
             :root-view root-view)
      (let [scene-graph (create-scene-graph root-view)]
        (swap! application-loop-state-atom
               assoc
               :scene-graph scene-graph)))
    bindings))

(defn create-bindings [root-view]
  (let [bindings (create-bindings-without-window root-view)]
    (with-bindings bindings
      (swap! application-loop-state-atom
             assoc
             :window (create-window (:window-width @application-loop-state-atom)
                                    (:window-height @application-loop-state-atom))))
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

  (swap! application-loop-state-atom
         assoc
         :scene-graph
         (loop [events (preserve-only-latest-event :mouse-moved events)
                scene-graph (:scene-graph @application-loop-state-atom)]
           (if (empty? events)
             scene-graph
             (if (or (= :close-requested (:type (first events)))
                     (keyboard/key-pattern-pressed? [#{:alt :meta} :w]
                                                    (first events)))
               nil
               (do (process-event! scene-graph
                                   (first events))
                   (recur (rest events)
                          (create-scene-graph (:root-view @application-loop-state-atom)))))))))

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
  ) ;; TODO: remove me


(defn application-loop-render! []
  ;; (logga/write 'application-loop-render!) ;; TODO: remove me

  (when (:scene-graph @application-loop-state-atom)
    #_(scene-graph/print-scene-graph (scene-graph/select-node-keys [:id :entity] (:scene-graph @application-loop-state-atom)))
    (window/with-gl (:window @application-loop-state-atom) gl
      (taoensso.tufte/p :render
                        (render (:scene-graph @application-loop-state-atom)
                                swing-root-renderer/render-scene-graph
                                gl)))
    (taoensso.tufte/p :swap-buffers
                      (window/swap-buffers (:window @application-loop-state-atom)))))

(defn start-application [root-view & {:keys [target-frame-rate
                                             on-exit]
                                      :or {target-frame-rate 60}}]
  (println "------------ start-window -------------")
  (with-bindings (create-bindings root-view)
    (thread "fungl application"
            (try (loop []

                   (handle-events! (read-events (window/event-channel (:window @application-loop-state-atom))
                                                target-frame-rate))
                   (application-loop-render!)

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
                         (swap! application-loop-state-atom
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
                                                     #_(layout/layout-scene-graph (:window-width @application-loop-state-atom)
                                                                                  (:window-height @application-loop-state-atom)))]

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

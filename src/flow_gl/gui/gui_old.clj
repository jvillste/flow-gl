(ns flow-gl.gui.gui-old
  (:require [clojure.core.async :as async]
            #_[flow-gl.tools.layoutable-inspector :as layoutable-inspector]
            [flow-gl.utils :as utils]
            [flow-gl.debug :as debug]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [drawable :as drawable]
                         [events :as events]
                         [quad-view :as quad-view]
                         [renderer :as renderer]
                         [window :as window]
                         [transformer :as transformer])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])



            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad-batch :as quad-batch]))
  (:use flow-gl.utils
        midje.sweet
        clojure.test))

(def ^:dynamic last-event-channel-atom (atom nil))

(defn redraw-last-started-view []
  (when-let [last-event-channel-atom @last-event-channel-atom]
    (async/put! last-event-channel-atom {:type :request-redraw})))






(defn layout-path-to-state-path [root-layout child-path]
  (loop [state-path (or (:state-path root-layout) [])
         rest-of-child-path child-path
         child-path []]
    (if-let [child-path-part (first rest-of-child-path)]
      (let [child-path (conj child-path child-path-part)]
        (if-let [new-state-path (get-in root-layout (conj child-path :state-path))]
          (recur new-state-path
                 (rest rest-of-child-path)
                 child-path)
          (recur state-path
                 (rest rest-of-child-path)
                 child-path)))
      state-path)))

(fact (layout-path-to-state-path {:children [{:state-path-part [:foo 1]
                                              :child {:child {:state-path-part [:bar]
                                                              :child {}}}}]}
                                 [:children 0 :child :child :child])
      => '(:foo 1 :bar))



(def ^:dynamic current-event-channel nil)
(def ^:dynamic current-frame-time)

(defn update-or-apply-in [map path function & arguments]
  (if (seq path)
    (apply update-in map path function arguments)
    (apply function map arguments)))

(defn apply-keyboard-event-handlers [state event]
  (reduce (fn [state focus-path-parts]
            (let [focus-path (apply concat focus-path-parts)]
              (if-let [keyboard-event-handler (get-in state (conj (vec focus-path) :handle-keyboard-event))]
                (update-or-apply-in state focus-path keyboard-event-handler event)
                state)))
          state
          (conj (path-prefixes (:focused-state-paths state))
                [])))



(defn set-focus-state [state focus-path-parts has-focus]
  (if (seq focus-path-parts)
    (loop [focus-path (first focus-path-parts)
           focus-path-parts (rest focus-path-parts)
           state state]
      (if (seq focus-path-parts)
        (recur (concat focus-path (first focus-path-parts))
               (rest focus-path-parts)
               (update-in state focus-path assoc :child-has-focus has-focus))
        (update-in state focus-path (fn [state]
                                      (let [focus-handler-key (if has-focus :on-focus-gained :on-focus-lost)]
                                        (-> (if-let [focus-handler (focus-handler-key state)]
                                              (focus-handler state)
                                              state)
                                            (assoc :has-focus has-focus)))))))
    state))



(fact (set-focus-state {:foo [{:bar {:baz :foobar}}
                              {:bar {:baz {:foo :bar}}}]}
                       [[:foo 1] [:bar] [:baz]]
                       true)
      => {:foo
          [{:bar {:baz :foobar}}
           {:child-has-focus true
            :bar {:child-has-focus true
                  :baz {:has-focus true
                        :foo :bar}}}]})





(defn set-mouse-over [state mouse-over-paths]
  (if (not (= mouse-over-paths (:mouse-over-paths state)))
    (move-hierarchical-state state mouse-over-paths :mouse-over-paths :mouse-over-child :mouse-over :on-mouse-enter :on-mouse-leave)
    state))

(defn apply-layout-event-handlers [state layout layout-path handler-key & arguments]
  (reduce (fn [state layout-path]
            (apply update-or-apply-in
                   state
                   (layout-path-to-state-path layout layout-path)
                   (get-in layout (conj layout-path handler-key))
                   arguments))
          state
          (filter (fn [layout-path]
                    (handler-key (get-in layout layout-path)))
                  (path-prefixes layout-path))))





(defn apply-layout-event-handlers-2 [state layout layout-path handler-key & arguments]
  (reduce (fn [state layout-path]
            (reduce (fn [state handler]
                      (apply handler
                             state
                             arguments))
                    state
                    (get-in layout (conj layout-path handler-key))))
          state
          (layout-path-to-handlers layout-path layout handler-key)))

(defn apply-mouse-over-layout-event-handlers [state layout new-mouse-over-layout-paths]

  (if (not (= new-mouse-over-layout-paths
              (:mouse-over-layout-paths state)))
    (-> state
        #_(apply-layout-event-handlers layout (:mouse-over-layout-paths state) :on-mouse-leave)
        #_(apply-layout-event-handlers layout new-mouse-over-layout-paths :on-mouse-enter)
        (apply-layout-event-handlers-3 layout (:mouse-over-layout-paths state) :handle-mouse-event-2 {:type :mouse-leave})
        (apply-layout-event-handlers-3 layout new-mouse-over-layout-paths :handle-mouse-event-2 {:type :mouse-enter})
        (assoc :mouse-over-layout-paths new-mouse-over-layout-paths))
    state))

(defn handle-event [state layout event]

  (cond

   (= event nil)
   state

   (= (:type event)
      :apply-to-view-state)
   ((:function event) state)

   (= (:source event)
      :mouse)
   (let [layout-paths-under-mouse (reverse (layout/layout-paths-in-coordinates layout (:x event) (:y event)))
         layout-path-under-mouse (last layout-paths-under-mouse)
         state (case (:type event)
                 :mouse-pressed (if (get-in layout (conj (vec layout-path-under-mouse) :on-drag))
                                  (assoc state
                                    :dragging-layout-path layout-path-under-mouse
                                    :previous-drag-x (:x event)
                                    :previous-drag-y (:y event))
                                  state)
                 :mouse-released (assoc state
                                   :dragging-layout-path nil)
                 :mouse-clicked (let [state-paths-under-mouse (layout-path-to-state-paths layout layout-path-under-mouse)]
                                  (-> state
                                      (cond-> (get-in state (concat (last state-paths-under-mouse)
                                                                    [:can-gain-focus]))
                                              (set-focus state-paths-under-mouse))
                                      (apply-layout-event-handlers layout layout-path-under-mouse :on-mouse-clicked)))
                 :mouse-dragged (if-let [dragging-layout-path (:dragging-layout-path state)]
                                  (-> (update-or-apply-in
                                       state
                                       (layout-path-to-state-path layout dragging-layout-path)
                                       (get-in layout (conj (vec dragging-layout-path) :on-drag))
                                       (- (:x event)
                                          (:previous-drag-x state))
                                       (- (:y event)
                                          (:previous-drag-y state)))
                                      (assoc :previous-drag-x (:x event)
                                             :previous-drag-y (:y event)))
                                  state)
                 :mouse-moved (let [state-paths-under-mouse (layout-path-to-state-paths layout layout-path-under-mouse)]
                                (-> state
                                    (set-mouse-over state-paths-under-mouse)
                                    (apply-mouse-over-layout-event-handlers layout layout-paths-under-mouse)))
                 state)]

     (when (= (:type event)
              :mouse-pressed)

       (println "layout paths"
                layout-paths-under-mouse
                (map (fn [path]
                       (type (get-in layout path)))
                     layout-paths-under-mouse))

       (println "layout path under mouse"
                layout-path-under-mouse
                (type (get-in layout layout-path-under-mouse))))

     (apply-layout-event-handlers-3 state layout layout-paths-under-mouse :handle-mouse-event-2 event)

     #_(reduce (fn [state layout-path]
                 (apply-layout-event-handlers-2 state layout layout-path :handle-mouse-event-2 event))
               state
               layout-paths-under-mouse)
     #_(-> state
           (apply-layout-event-handlers layout layout-path-under-mouse :handle-mouse-event event)
           (apply-layout-event-handlers-2 layout layout-path-under-mouse :handle-mouse-event-2 event)))

   (events/key-pressed? event :tab)
   (set-focus state (or (next-focus-path-parts state (:focused-state-paths state))
                        (initial-focus-path-parts state)))

   (= (:type event)
      :close-requested)
   (do (println "got :close-requested event, returning nil as view state")
       nil)

   (= (:type event)
      :resize-requested)
   state

   :default
   (apply-keyboard-event-handlers-2 state
                                    event)))

(defn request-close [event-channel]
  (async/put! event-channel {:type :close-requested}))

(defn close-control-channels [state]
  (async/close! (:control-channel state))
  (doseq [state (vals (:child-states state))]
    (close-control-channels state)))



(defn drain [channel timeout]
  (loop [values (if (not timeout)
                  [(async/<!! channel)]
                  (async/alt!! (async/timeout timeout) ([_] [])
                               channel ([value] [value])))]
    (async/alt!! (async/timeout 0) ([_] values)
                 channel ([value] (recur (conj values value)))
                 :priority true)))

(defn render-layout [layout render-tree-state-atom window]
  #_(let [render-trees (transformer/render-trees-for-layout layout)]
      (window/with-gl window gl
        (opengl/clear gl 0 0 0 1)
        (let [{:keys [width height]} (opengl/size gl)]
          (swap! render-tree-state-atom
                 (fn [render-tree-state]
                   (let [[render-tree-state drawables] (transformer/transform-tree render-tree-state
                                                                                   {:transformers [(transformer/->RenderTransformer :root)]
                                                                                    :children render-trees
                                                                                    :width width
                                                                                    :height height
                                                                                    :x 0
                                                                                    :y 0}
                                                                                   gl)]
                     render-tree-state)))))))

(defn start-view [constructor view]
  (let [control-channel (async/chan)
        window (jogl-window/create 500
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize)
        event-channel (window/event-channel window)
        root-view-context {:state-path []
                           :event-channel event-channel}]

    (reset! last-event-channel-atom event-channel )

    (try
      (let [initial-state (constructor root-view-context
                                       control-channel)
            initial-state (binding [current-event-channel event-channel
                                    current-frame-time (System/currentTimeMillis)]
                            (-> (view root-view-context
                                      initial-state
                                      nil)
                                :state))
            initial-state (set-focus initial-state
                                     (initial-focus-path-parts initial-state))
            initial-state (assoc initial-state :control-channel control-channel)
            render-tree-state-atom (atom nil)]


        (loop [state initial-state
               last-frame-started (System/currentTimeMillis)]

          (let [target-frames-per-second 60]
            (Thread/sleep (max 0
                               (- (/ 1000 target-frames-per-second)
                                  (- (System/currentTimeMillis)
                                     last-frame-started)))))

          (let [frame-started (System/currentTimeMillis)]
            (flow-gl.debug/debug-timed "got new state")

            (if (:close-requested state)
              (do (close-control-channels state)
                  (window/close window))

              (let [{:keys [state layoutable sleep-time]} (binding [current-event-channel event-channel
                                                                    current-frame-time frame-started]
                                                            (view root-view-context
                                                                  state
                                                                  nil))
                    width (window/width window)
                    height (window/height window)
                    [state layout] (layout/layout layoutable
                                                  state
                                                  width
                                                  height)
                    layout (-> layout
                               (assoc :x 0
                                      :y 0
                                      :width width
                                      :height height)
                               (layout/add-out-of-layout-hints))]

                (render-layout layout render-tree-state-atom window)

                (let [new-state (binding [current-event-channel event-channel]
                                  (reduce #(handle-event %1 layout %2)
                                          state
                                          (drain event-channel sleep-time)))]
                  (if new-state
                    (recur new-state
                           frame-started)
                    (do (close-control-channels state)
                        (window/close window)))))))))

      (catch Exception e
        (window/close window)
        (throw e)))))




(defn add-mouse-event-handler [layoutable handler]
  (assoc layoutable
    :handle-mouse-event-2 (conj (or (:handle-mouse-event-2 layoutable)
                                    [])
                                handler)))

(defn add-mouse-event-handler-with-context [layoutable view-context handler]
  (add-mouse-event-handler layoutable (fn [state event]
                                        (update-or-apply-in state (:state-path view-context) handler event))))

(defn on-mouse-clicked [layoutable view-context handler & arguments]
  (add-mouse-event-handler-with-context layoutable view-context (fn [state event]
                                                                  (if (= :mouse-clicked (:type event))
                                                                    (apply handler state event arguments)
                                                                    state))))

(defn on-mouse-event [layoutable event-type view-context handler & arguments]
  (add-mouse-event-handler-with-context layoutable view-context (fn [state event]
                                                                  (if (= event-type (:type event))
                                                                    (apply handler state event arguments)
                                                                    state))))

(defn seq-focus-handlers [child-seq-key]
  {:first-focusable-child (fn [_] [child-seq-key 0])
   :next-focusable-child (fn [this [_ child-index]]
                           (let [new-child-index (inc child-index)]
                             (if (= new-child-index
                                    (count (child-seq-key this)))
                               nil
                               [child-seq-key (inc child-index)])))})



(defn call-view-for-sequence [parent-state view-function key]
  (map-indexed (fn [index child-state]
                 (assoc (view-function child-state)
                   :state-path-part [key index]))
               (key parent-state)))






(def child-focus-handlers
  {:first-focusable-child (fn [state]
                            [:child-states (first (:children state))])
   :next-focusable-child (fn [this currently-focused-path-part]
                           (let [child-index (first (positions #{currently-focused-path-part} (map #(conj [:child-states] %)
                                                                                                   (:children this))))]
                             (let [new-child-index (inc child-index)]
                               (if (= new-child-index
                                      (count (:children this)))
                                 nil
                                 [:child-states (get (:children this)
                                                     new-child-index)]))))})


(fact (update-or-apply-in {:foo {:baz :bar}} [] assoc :x :y) => {:foo {:baz :bar}, :x :y}
      (update-or-apply-in {:foo {:baz :bar}} [:foo] assoc :x :y) => {:foo {:baz :bar, :x :y}})

(def ^:dynamic current-view-state-atom)

(defn call-named-view
  ([parent-view-context view constructor child-id state-overrides]
     (call-named-view parent-view-context view constructor child-id [] state-overrides))

  ([parent-view-context view constructor child-id constructor-parameters state-overrides]
     (let [state-path-part [:child-states child-id]
           state-path (concat (:state-path parent-view-context) state-path-part)

           constructor ((-> parent-view-context :application-state :constructor-decorator)
                        constructor)

           view-context ((-> parent-view-context :application-state :view-context-decorator)
                         (assoc parent-view-context
                           :state-path state-path))

           state (-> (or (get-in @current-view-state-atom state-path-part)
                         (apply constructor view-context
                                constructor-parameters))
                     (conj state-overrides))

           view ((-> parent-view-context :application-state :view-decorator)
                 view)

           {:keys [state layoutable]} (view view-context
                                            state)]

       (swap! current-view-state-atom assoc-in state-path-part state)
       (swap! current-view-state-atom add-child child-id)
       (assoc layoutable
         :state-path state-path))))



(defn call-anonymous-view [view constructor state-overrides]
  (call-named-view view
                   constructor
                   [:child (count (:children @current-view-state-atom))]
                   state-overrides))

(defn set-wake-up [view-context sleep-time]
  (swap! (:sleep-time-atom view-context)
         (fn [old-sleep-time]
           (if old-sleep-time
             (if sleep-time
               (min old-sleep-time sleep-time)
               old-sleep-time)
             sleep-time))))

(defn apply-to-current-view-state [function & parameters]
  (apply swap! current-view-state-atom function parameters))



(defmacro def-view [name parameters & body]
  (let [[view-context-parameter state-parameter] parameters]
    `(defn ~name [view-context# state#]
       (binding [current-view-state-atom (atom state#)]
         (let [layoutable# (let [~view-context-parameter view-context#
                                 ~state-parameter state#]
                             ~@body)]
           {:state @current-view-state-atom
            :layoutable layoutable#})))))

(defmacro def-control [name constructor view]
  (let [view-name (symbol (str name "-view"))
        constructor-name (symbol (str "create-" name))]
    `(do (def-view ~view-name ~@view)
         (defn ~constructor-name ~@constructor)
         (defn ~name
           ([parent-view-context# state-overrides#]
              (call-anonymous-view parent-view-context# ~view-name ~constructor-name state-overrides#))
           ([parent-view-context# id# state-overrides#]
              (call-named-view parent-view-context# ~view-name ~constructor-name id# state-overrides#))
           ([parent-view-context# id# state-overrides# constructor-parameters#]
              (call-named-view parent-view-context# ~view-name ~constructor-name id# constructor-parameters# state-overrides#))))))





(defn apply-to-state-2 [state-path function]
  (async/go (async/>! current-event-channel
                      (create-apply-to-view-state-event (fn [state]
                                                          (update-or-apply-in state state-path function))))))

(defn transact [state-path event-channel function]
  (async/go (async/>! event-channel
                      (create-apply-to-view-state-event (fn [state]
                                                          (update-or-apply-in state state-path function))))))

(defmacro apply-to-current-state [[state-parameter & parameters] & body]
  `(let [state-path# current-state-path]
     (fn [~@parameters]
       (apply-to-state-2 state-path# (fn [~state-parameter]
                                       ~@body)))))



(defmacro with-view-context [[state-path-parameter event-channel-parameter & parameters] & body]
  `(let [~state-path-parameter current-state-path
         ~event-channel-parameter current-event-channel]
     (fn [~@parameters]
       ~@body)))

(defn focus-index [state]
  (loop [index 0]
    (if (:has-focus (get state (get (:children state) index)))
      index
      (if (< index
             (count (:children state)))
        (recur (inc index))
        nil))))

(fact (focus-index {0 {}
                    1 {:has-focus true}
                    :children [0 1]})
      => 1

      (focus-index {0 {}
                    1 {}
                    :children [0 1]})
      => nil)


(run-all-tests)
(ns flow-gl.gui.quad-gui
  (:require [clojure.core.async :as async]
            #_[flow-gl.tools.layoutable-inspector :as layoutable-inspector]
            [flow-gl.utils :as utils]
            [flow-gl.debug :as debug]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            #_(flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]))
  (:use flow-gl.utils
        midje.sweet
        clojure.test))

(def ^:dynamic last-event-channel-atom (atom nil))

(defn redraw-last-started-view []
  (when-let [last-event-channel-atom @last-event-channel-atom]
    (async/put! last-event-channel-atom {:type :request-redraw})))


(defn path-prefixes [path]
  (loop [prefixes []
         prefix []
         xs path]
    (if-let [x (first xs)]
      (let [prefix (conj prefix x)]
        (recur (conj prefixes prefix)
               prefix
               (rest xs)))
      prefixes)))

(fact (path-prefixes [[1 2] [3] [4]])
      => [[[1 2]]
          [[1 2] [3]]
          [[1 2] [3] [4]]])

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

(defn layout-path-to-state-paths [root-layout child-path]
  (loop [state-paths (if-let [state-path (:state-path root-layout)]
                       [state-path]
                       [])
         rest-of-child-path child-path
         child-path []]
    (if-let [child-path-part (first rest-of-child-path)]
      (let [child-path (conj child-path child-path-part)]
        (if-let [new-state-path (get-in root-layout (conj child-path :state-path))]
          (recur (conj state-paths new-state-path)
                 (rest rest-of-child-path)
                 child-path)
          (recur state-paths
                 (rest rest-of-child-path)
                 child-path)))
      state-paths)))

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
          (conj (path-prefixes (:focus-path-parts state))
                [])))

(defn apply-keyboard-event-handlers-2 [state event]
  (loop [state state
         focus-path-prefixes (concat [[]]
                                     (path-prefixes (:focus-path-parts state)))]
    (if-let [focus-prefix (first focus-path-prefixes)]
      (let [focus-path (apply concat focus-prefix)]
        (if-let [keyboard-event-handler (get-in state (conj (vec focus-path) :handle-keyboard-event))]
          (if (seq focus-path)
            (let [[child-state continue] (keyboard-event-handler (get-in state focus-path)
                                                                 event)
                  state (assoc-in state focus-path child-state)]
              (if continue
                (recur state
                       (rest focus-path-prefixes))
                state))
            (let [[state continue] (keyboard-event-handler state event)]
              (if continue
                (recur state
                       (rest focus-path-prefixes))
                state)))
          (recur state
                 (rest focus-path-prefixes))))
      state)))

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


(defn set-hierarchical-state [state paths new-value child-state-key state-key state-gained-key state-lost-key]
  (if (seq paths)
    (loop [paths paths
           state state]
      (if (seq (rest paths))
        (recur (rest paths)
               (update-in state (first paths) assoc child-state-key new-value))
        (update-in state (first paths) (fn [state]
                                         (let [focus-handler-key (if new-value state-gained-key state-lost-key)]
                                           (-> (if-let [focus-handler (focus-handler-key state)]
                                                 (focus-handler state)
                                                 state)
                                               (assoc state-key new-value)))))))
    state))

(defn seq-focus-handlers [child-seq-key]
  {:first-focusable-child (fn [_] [child-seq-key 0])
   :next-focusable-child (fn [this [_ child-index]]
                           (let [new-child-index (inc child-index)]
                             (if (= new-child-index
                                    (count (child-seq-key this)))
                               nil
                               [child-seq-key (inc child-index)])))})

(defn initial-focus-path-parts [state]
  (loop [state state
         focus-path-parts []]
    (if-let [focus-path-part (when-let [first-focusable-child-function (:first-focusable-child state)]
                               (first-focusable-child-function state))]
      (recur (get-in state focus-path-part)
             (conj focus-path-parts focus-path-part))
      focus-path-parts)))

(fact (initial-focus-path-parts {:first-focusable-child (fn [_] [:children 1])
                                 :children [{:first-focusable-child (fn [_] [:children 0])
                                             :children [{}]}
                                            {:first-focusable-child (fn [_] [:children 0])
                                             :children [{}]}]})
      => [[:children 1] [:children 0]])

(defn next-focus-path-parts [state previous-focus-path-parts]
  (when (seq previous-focus-path-parts)
    (let [parent-focus-path-parts (vec (drop-last previous-focus-path-parts))
          focus-parent (get-in state (apply concat parent-focus-path-parts))]
      (if-let [next-focus-path-part ((:next-focusable-child focus-parent) focus-parent (last previous-focus-path-parts))]
        (concat parent-focus-path-parts
                [next-focus-path-part]
                (initial-focus-path-parts (get-in state
                                                  (apply concat
                                                         (conj parent-focus-path-parts
                                                               next-focus-path-part)))))
        (next-focus-path-parts state parent-focus-path-parts)))))

(fact (let [state (conj (seq-focus-handlers :children1)
                        {:children1 [(conj (seq-focus-handlers :children2)
                                           {:children2 [{}{}{}]})
                                     (conj (seq-focus-handlers :children2)
                                           {:children2 [{}{}(conj (seq-focus-handlers :children3)
                                                                  {:children3 [{}{}{}]})]})]})]

        (next-focus-path-parts state [[:children1 0] [:children2 1]])  => [[:children1 0] [:children2 2]]
        (next-focus-path-parts state [[:children1 0] [:children2 2]])  => [[:children1 1] [:children2 0]]
        (next-focus-path-parts state [[:children1 1] [:children2 1]])  => [[:children1 1] [:children2 2] [:children3 0]]
        (next-focus-path-parts state [[:children1 1] [:children2 2] [:children3 2]])  => nil))

(defn render-layout [window gpu-state-atom layout]
  (let [removed-texels (get-in @gpu-state-atom [:quad-batch :removed-texels])
        allocated-texels (get-in @gpu-state-atom [:quad-batch :allocated-texels])]
    (debug/set-metric :texel-fill-ratio (/ removed-texels allocated-texels) :ratio? true)
    (debug/set-metric :removed-texels removed-texels)
    (debug/set-metric :allocated-texels allocated-texels))


  (window/set-display window gl
                      (let [size (opengl/size gl)]
                        (opengl/clear gl 0 0 0 1)
                        (reset! gpu-state-atom
                                (quad-view/draw-layout @gpu-state-atom
                                                       layout
                                                       (:width size)
                                                       (:height size)
                                                       gl)))))

(defn move-hierarchical-state [state paths previous-path-parts-key child-state-key state-key state-gained-key state-lost-key]
  (-> state
      (set-hierarchical-state (previous-path-parts-key state) false child-state-key state-key state-gained-key state-lost-key)
      (set-hierarchical-state paths true child-state-key state-key state-gained-key state-lost-key)
      (assoc previous-path-parts-key paths)))

(defn set-focus [state focus-paths]
  (move-hierarchical-state state focus-paths :focus-path-parts :child-has-focus :has-focus :on-focus-gained :on-focus-lost))

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
          (filter (fn [layout-path]
                    (handler-key (get-in layout layout-path)))
                  (path-prefixes layout-path))))

(defn apply-mouse-over-layout-event-handlers [state layout new-mouse-over-layout-path]

  (if (not (= new-mouse-over-layout-path
              (:mouse-over-layout-path state)))
    (do (println "mouse over" new-mouse-over-layout-path (type (get-in layout new-mouse-over-layout-path)))
        (-> state
            (apply-layout-event-handlers layout (:mouse-over-layout-path state) :on-mouse-leave)
            (apply-layout-event-handlers layout new-mouse-over-layout-path :on-mouse-enter)
            (apply-layout-event-handlers-2 layout (:mouse-over-layout-path state) :handle-mouse-event-2 {:type :mouse-leave})
            (apply-layout-event-handlers-2 layout new-mouse-over-layout-path :handle-mouse-event-2 {:type :mouse-enter})
            (assoc :mouse-over-layout-path new-mouse-over-layout-path)))
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
   (let [layout-path-under-mouse (last (layout/layout-paths-in-coordinates layout (:x event) (:y event)))
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
                                    (apply-mouse-over-layout-event-handlers layout layout-path-under-mouse)))
                 state)]

     (-> state
         (apply-layout-event-handlers layout layout-path-under-mouse :handle-mouse-event event)
         (apply-layout-event-handlers-2 layout layout-path-under-mouse :handle-mouse-event-2 event)))

   (events/key-pressed? event :tab)
   (set-focus state (or (next-focus-path-parts state (:focus-path-parts state))
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

(defn start-view [constructor view]
  (let [event-channel (async/chan 50)
        control-channel (async/chan)
        window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :reshape opengl/resize
                              :event-channel event-channel)
        root-view-context {:state-path []
                           :event-channel event-channel}]

    (reset! last-event-channel-atom event-channel)


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
            gpu-state-atom (atom (window/with-gl window gl (quad-view/create gl)))]


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

                #_(flow-gl.debug/debug-timed "waiting")
                #_(Thread/sleep 500)
                (flow-gl.debug/debug-timed-and-return "render"
                                                      (render-layout window gpu-state-atom layout))

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

(defn create-apply-to-view-state-event [function]
  {:type :apply-to-view-state
   :function function})



(defn add-mouse-event-handler-with-context [layoutable view-context handler]
  (assoc layoutable
    :handle-mouse-event-2 (conj (or (:handle-mouse-event-2 layoutable)
                                    [])
                                (fn [state event]
                                  (update-or-apply-in state (:state-path view-context) handler event)))))

(defn on-mouse-clicked [layoutable view-context handler & arguments]
  (add-mouse-event-handler-with-context layoutable view-context (fn [state event]
                                                                  (if (= :mouse-clicked (:type event))
                                                                    (apply handler state (:time event) arguments)
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


(defn add-child [state path]
  (update-in state [:children] conj path))

(defn reset-children [state]
  (-> state
      (assoc :old-children (or (:children state)
                               []))
      (assoc :children [])))



(defn remove-unused-children [state]
  (let [child-set (set (:children state))
        children-to-be-removed (->> (:old-children state)
                                    (filter (complement child-set)))]
    (reduce (fn [state child-to-be-removed]
              (do (close-control-channels (get-in state [:child-states child-to-be-removed]))
                  (update-in state [:child-states] dissoc child-to-be-removed)))
            state
            children-to-be-removed)))

#_(fact (let [state-1 (-> {}
                          (reset-children)

                          (assoc-in [:child-states :1] :foo)
                          (add-child :1)

                          (assoc-in [:child-states :2] :foo)
                          (add-child :2)

                          (remove-unused-children))]

          state-1 => {:child-states {:1 :foo, :2 :foo}, :children [:1 :2], :old-children []}

          (-> state-1
              (reset-children)

              (assoc-in [:child-states :2] :foo2)
              (add-child :2)

              (remove-unused-children)) => {:child-states {:2 :foo2}, :children [:2], :old-children [:1 :2]}))


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

(def ^:dynamic current-state-path [])
(def ^:dynamic current-view-state-atom)
(def ^:dynamic sleep-time-atom)

(defn call-named-view
  ([view constructor child-id state-overrides]
     (call-named-view view constructor child-id [] state-overrides))

  ([view constructor child-id constructor-parameters state-overrides]
     (let [state-path-part [:child-states child-id]
           state-path (concat current-state-path state-path-part)
           state (-> (or (get-in @current-view-state-atom state-path-part)
                         (let [control-channel (async/chan)]
                           (-> (apply constructor {:state-path state-path
                                                   :event-channel current-event-channel}
                                      control-channel
                                      constructor-parameters)
                               (assoc :control-channel control-channel))))
                     (conj state-overrides))

           {:keys [state layoutable]} (binding [current-state-path state-path]
                                        (view {:state-path state-path
                                               :event-channel current-event-channel}
                                              state
                                              @sleep-time-atom))]
       (swap! current-view-state-atom assoc-in state-path-part state)
       (swap! current-view-state-atom add-child child-id)
       (assoc layoutable
         :state-path-part state-path-part
         :state-path state-path))))

(defn call-anonymous-view [view constructor state-overrides]
  (call-named-view view
                   constructor
                   [:child (count (:children @current-view-state-atom))]
                   state-overrides))

(defmacro with-children [state visual]
  `(binding [current-view-state-atom (atom (reset-children ~state))]
     (let [visual-value# ~visual]
       {:state (remove-unused-children @current-view-state-atom)
        :layoutable visual-value#})))

(defmacro with-animation [sleep-time & body]
  `(binding [sleep-time-atom (atom ~sleep-time)]
     (let [value# (do ~@body)]
       (assoc value# :sleep-time @sleep-time-atom))))

(defn set-wake-up [sleep-time]
  (swap! sleep-time-atom (fn [old-sleep-time]
                           (if old-sleep-time
                             (if sleep-time
                               (min old-sleep-time sleep-time)
                               old-sleep-time)
                             sleep-time))))

(defn apply-to-current-view-state [function & parameters]
  (apply swap! current-view-state-atom function parameters))

(defmacro def-view [name parameters & body]
  (let [[view-context-parameter state-parameter] parameters]
    `(defn ~name [view-context# state# sleep-time#]
       (with-animation sleep-time#
         (with-children state#
           (let [~view-context-parameter view-context#
                 ~state-parameter state#]
             ~@body))))))

(defmacro def-control [name constructor view]
  (let [view-name (symbol (str name "-view"))
        constructor-name (symbol (str "create-" name))]
    `(do (def-view ~view-name ~@view)
         (defn ~constructor-name ~@constructor)
         (defn ~name
           ([state-overrides#]
              (call-anonymous-view ~view-name ~constructor-name state-overrides#))
           ([id# state-overrides#]
              (call-named-view ~view-name ~constructor-name id# state-overrides#))
           ([id# state-overrides# constructor-parameters#]
              (call-named-view ~view-name ~constructor-name id# constructor-parameters# state-overrides#))))))


(defn apply-to-state [view-context function & arguments]
  (async/go (async/>! (:event-channel view-context)
                      (create-apply-to-view-state-event (fn [state]
                                                          (if (get-in state (:state-path view-context))
                                                            (apply update-or-apply-in state (:state-path view-context) function arguments)
                                                            (flow-gl.debug/debug-timed "tried to apply to empty state" (vec (:state-path view-context)))))))))


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

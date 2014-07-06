(ns flow-gl.gui.quad-gui
  (:require [clojure.core.async :as async]
            [flow-gl.tools.layoutable-inspector :as layoutable-inspector]
            [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view])

            (flow-gl.graphics [command :as command]
                              [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]))
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl
        clojure.test))

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
  (loop [state-path []
         rest-of-child-path child-path
         child-path []]
    (if-let [child-path-part (first rest-of-child-path)]
      (let [child-path (conj child-path child-path-part)]
        (if-let [state-path-part (get-in root-layout (conj child-path :state-path-part))]
          (recur (concat state-path state-path-part)
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

(defn layout-path-to-state-path-parts [root-layout whole-layout-path]
  (loop [state-path-parts []
         remaining-layout-path whole-layout-path
         layout-path []]
    (if-let [layout-path-part (first remaining-layout-path)]
      (let [new-layout-path (conj layout-path layout-path-part)]
        (if-let [state-path-part (get-in root-layout (conj new-layout-path :state-path-part))]
          (recur (conj state-path-parts state-path-part)
                 (rest remaining-layout-path)
                 new-layout-path)
          (recur state-path-parts
                 (rest remaining-layout-path)
                 new-layout-path)))
      state-path-parts)))

(fact (layout-path-to-state-path-parts {:children [{:state-path-part [:foo 1]
                                                    :child {:child {:state-path-part [:bar]
                                                                    :child {}}}}]}
                                       [:children 0 :child :child :child])
      => [[:foo 1] [:bar]])

(def ^:dynamic current-event-channel nil)
(def ^:dynamic current-view-state-atom)

(defn apply-mouse-event-handlers [state layout layout-path-under-mouse event]
  (reduce (fn [state layout-path]
            (update-in state
                       (layout-path-to-state-path layout layout-path)
                       (get-in layout (conj layout-path :handle-mouse-event))
                       event))
          state
          (filter (fn [layout-path]
                    (:handle-mouse-event (get-in layout layout-path)))
                  (path-prefixes layout-path-under-mouse))))

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
        (update-in state focus-path assoc :has-focus has-focus)))
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

(defn render-layout [window gpu-state layout]
  (window/with-gl window gl
    (opengl/clear gl 0 0 0 1)
    (quad-view/draw-layout gpu-state
                           (assoc layout
                             :x 0 :y 0)
                           (window/width window)
                           (window/height window)
                           gl)))

(defn set-focus [state focus-path-parts]
  (-> state
      (set-focus-state (:focus-path-parts state) false)
      (set-focus-state focus-path-parts true)
      (assoc :focus-path-parts focus-path-parts)))

(defn handle-event [state layout event]
  (cond
   (= (:type event)
      :apply-to-view-state)
   ((:function event) state)

   (= (:source event)
      :mouse)
   (let [layout-paths-under-mouse (layout/layout-paths-in-coordinates layout (:x event) (:y event))
         layout-path-under-mouse (last layout-paths-under-mouse)]

     (if (= (:type event)
            :mouse-clicked)
       (let [focus-path-parts (layout-path-to-state-path-parts layout layout-path-under-mouse)]
         (let [updated-state (set-focus state focus-path-parts)]
           (apply-mouse-event-handlers updated-state
                                       layout
                                       layout-path-under-mouse
                                       event)))
       (apply-mouse-event-handlers state
                                   layout
                                   layout-path-under-mouse
                                   event)))

   (events/key-pressed? event :tab)
   (set-focus state (or (next-focus-path-parts state (:focus-path-parts state))
                        (initial-focus-path-parts state)))

   (= (:type event)
      :close-requested)
   (do (println "got :close-requested event, returning nil as view state")
       nil)

   :default
   (apply-keyboard-event-handlers-2 state
                                    event)))



(defn start-view [initial-state view process-starter]
  (let [event-channel (async/chan 10)
        window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :reshape opengl/resize
                              :event-channel event-channel)
        [initial-state _] (binding [current-event-channel event-channel]
                            (view initial-state))
        initial-state (set-focus initial-state
                                 (initial-focus-path-parts initial-state))
        initial-state (assoc initial-state :control-channel (async/chan))]

    (try
      (process-starter [] event-channel (:control-channel initial-state) initial-state)
      (loop [gpu-state (window/with-gl window gl (quad-view/create gl))
             state initial-state]
                                        ;(println "state is " state)
        (if (:close-requested state)
          (window/close window)

          (let [[state visual] (binding [current-event-channel event-channel]
                                 (view state))
                layout (layout/layout visual
                                      (window/width window)
                                      (window/height window))
                new-gpu-state (render-layout window gpu-state layout)
                event (async/<!! event-channel)
                new-state (binding [current-event-channel event-channel]
                            (handle-event state
                                          layout
                                          event))]
            (if new-state
              (recur new-gpu-state new-state)
              (window/close window)))))
      (catch Exception e
        (window/close window)
        (throw e)))

    (async/go (async/>! (:control-channel initial-state) :close))))

(defn create-apply-to-view-state-event [function]
  {:type :apply-to-view-state
   :function function})

(defn on-mouse-clicked [layoutable handler]
  (assoc layoutable
    :handle-mouse-event (fn [state event]
                          (if (= :mouse-clicked (:type event))
                            (handler state)
                            state))))

(defn seq-focus-handlers [child-seq-key]
  {:first-focusable-child (fn [_] [child-seq-key 0])
   :next-focusable-child (fn [this [_ child-index]]
                           (let [new-child-index (inc child-index)]
                             (if (= new-child-index
                                    (count (child-seq-key this)))
                               nil
                               [child-seq-key (inc child-index)])))})



(def ^:dynamic current-state-path [])

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
              (update-in state [:child-states] dissoc child-to-be-removed))
            state
            children-to-be-removed)))

(fact (let [state-1 (-> {}
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
                           (println "currently-focused-path-part " currently-focused-path-part)
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

(defn call-view
  ([view-specification state-override]
     (call-view [:child (count (:children @current-view-state-atom))]
                view-specification
                state-override))

  ([child-id {:keys [initial-state view]} state-override]
     (let [state-path-part [:child-states child-id]
           old-state (or (get-in @current-view-state-atom state-path-part)
                         initial-state)
           new-state (conj old-state state-override)
           [new-state child-visual] (binding [current-state-path (concat current-state-path state-path-part)]
                                      (view new-state))]
       (swap! current-view-state-atom assoc-in state-path-part new-state)
       (swap! current-view-state-atom add-child child-id)
       (assoc child-visual
         :state-path-part state-path-part))))

(defmacro with-children [state visual]
  `(binding [current-view-state-atom (atom (reset-children ~state))]
     (let [visual-value# ~visual]
       [(remove-unused-children @current-view-state-atom)
        visual-value#])))

(defmacro def-view [name parameters visual]
  `(defn ~name ~parameters
     (with-children ~(first parameters)
       ~visual)))

(defn apply-to-state [state-path function]
  (async/go (async/>! current-event-channel
                      (create-apply-to-view-state-event (fn [state]
                                                          (update-or-apply-in state state-path function))))))

(defn transact [state-path event-channel function]
  (async/go (async/>! event-channel
                      (create-apply-to-view-state-event (fn [state]
                                                          (update-or-apply-in state state-path function))))))

(defmacro apply-to-current-state [[state-parameter & parameters] body]
  `(let [state-path# current-state-path]
     (fn [~@parameters]
       (apply-to-state state-path# (fn [~state-parameter]
                                     ~body)))))

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

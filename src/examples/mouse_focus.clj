(ns examples.mouse-focus
  (:require [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events])

            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]))
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl))

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
      => [[[1 2]] [[1 2] [3]] [[1 2] [3] [4]]])

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

(defn apply-keyboard-event-handlers [state focus-path-parts event]
  (reduce (fn [state focus-path-parts]
            (let [focus-path (apply concat focus-path-parts)]
              (if-let [keyboard-event-handler (get-in state (conj (vec focus-path) :handle-keyboard-event))]
                (update-in state
                           focus-path
                           keyboard-event-handler
                           event)
                state)))
          state
          (path-prefixes focus-path-parts)))

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
  {:first-focusable-child [child-seq-key 0]
   :next-focusable-child (fn [this [_ child-index]]
                           (let [new-child-index (inc child-index)]
                             (if (= new-child-index
                                    (count (child-seq-key this)))
                               nil
                               [child-seq-key (inc child-index)])))})

(defn initial-focus-path-parts [state]
  (loop [state state
         focus-path-parts []]
    (if-let [focus-path-part (:first-focusable-child state)]
      (recur (get-in state focus-path-part)
             (conj focus-path-parts focus-path-part))
      focus-path-parts)))

(fact (initial-focus-path-parts {:first-focusable-child [:children 1]
                                 :children [{:first-focusable-child [:children 0]
                                             :children [{}]}
                                            {:first-focusable-child [:children 0]
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

(defn start-view [view event-queue event-handler initial-state]
  (let [window (window/create 300
                              400
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (let [initial-focus-path-parts (initial-focus-path-parts initial-state)]
        (loop [state (-> initial-state
                         (set-focus-state initial-focus-path-parts true))
               previous-state {}
               previous-focus-path-parts initial-focus-path-parts
               cached-runnables {}]

          (let [layoutable (view state)
                layout (layout/layout layoutable
                                      (window/width window)
                                      (window/height window))
                drawing-commands (doall (drawable/drawing-commands layout))
                cached-runnables-atom (atom cached-runnables)
                unused-commands-atom (atom nil)]

            (when (not= state previous-state)
              (window/render window gl
                             (opengl/clear gl 0 0 0 1)
                             (doseq [runnable (doall (reduce (fn [runnables command]
                                                               (if (contains? @cached-runnables-atom command)
                                                                 (conj runnables (get @cached-runnables-atom command))
                                                                 (let [runnable (command/create-runner command gl)]
                                                                   (swap! cached-runnables-atom assoc command runnable)
                                                                   (conj runnables runnable))))
                                                             []
                                                             drawing-commands))]

                               ;;(println (type runnable))
                               (command/run runnable gl))

                             (let [unused-commands (filter (complement (apply hash-set drawing-commands))
                                                           (keys @cached-runnables-atom))]
                               (doseq [unused-command unused-commands]
                                 (if-let [unused-runnable (get unused-command @cached-runnables-atom)]
                                   (command/delete unused-runnable gl)))

                               (reset! unused-commands-atom unused-commands))))

            (let [event (event-queue/dequeue-event-or-wait event-queue)
                  live-commands (apply dissoc @cached-runnables-atom @unused-commands-atom)]
              (cond
               (= (:source event)
                  :mouse)
               (let [layout-paths-under-mouse (layout/layout-paths-in-coordinates layout (:x event) (:y event))
                     layout-path-under-mouse (last layout-paths-under-mouse)]

                 (if (= (:type event)
                        :mouse-clicked)
                   (let [focus-path-parts (layout-path-to-state-path-parts layout layout-path-under-mouse)]

                     (recur (-> state
                                (set-focus-state previous-focus-path-parts false)
                                (set-focus-state focus-path-parts true)
                                (apply-mouse-event-handlers layout
                                                            layout-path-under-mouse
                                                            event))
                            state
                            focus-path-parts
                            live-commands))

                   (recur (apply-mouse-event-handlers state
                                                      layout
                                                      layout-path-under-mouse
                                                      event)
                          state
                          previous-focus-path-parts
                          live-commands)))

               (events/key-pressed? event :tab)
               (let [all-handlers (map (partial layout-path-to-state-path-parts layout)
                                       (layout/layout-paths-with-keyboard-event-handlers layout))
                     focus-index (first (positions #{previous-focus-path-parts} all-handlers))
                     next-focus-index (if (= (+ 1 focus-index) (count all-handlers))
                                        0
                                        (inc focus-index))
                     next-focus-path-parts (get (vec all-handlers) next-focus-index)]
                 (println "layout " layout)
                 (println "layout paths " (layout/layout-paths-with-keyboard-event-handlers layout))
                 (println "handlers " all-handlers)
                 (println "focus index " focus-index)
                 (println "next focus index " next-focus-index)
                 (println "next-focus-path-parts " next-focus-path-parts)

                 (recur (-> state
                            (set-focus-state previous-focus-path-parts false)
                            (set-focus-state next-focus-path-parts true))
                        state
                        next-focus-path-parts
                        live-commands))

               (= (:type event)
                  :close-requested)
               (do (println "closing")
                   (window/close window))

               :default
               (recur #_(event-handler state event)
                      (apply-keyboard-event-handlers state
                                                     previous-focus-path-parts
                                                     event)
                      state
                      previous-focus-path-parts
                      live-commands))))))


      (catch Exception e
        (window/close window)
        (throw e)))))

(defn on-mouse-clicked [layoutable handler]
  (assoc layoutable
    :handle-mouse-event (fn [state event]
                          (if (= :mouse-clicked (:type event))
                            (handler state)
                            state))))

(defn handle-click-counter-keyboard-event [state event]
  (cond (events/key-pressed? event :enter)
        (-> (update-in state [:count] inc))

        :default
        state))


(defn seq-focus-handlers [child-seq-key]
  {:first-focusable-child [child-seq-key 0]
   :next-focusable-child (fn [this [_ child-index]]
                           (let [new-child-index (inc child-index)]
                             (if (= new-child-index
                                    (count (child-seq-key this)))
                               nil
                               [child-seq-key (inc child-index)])))})

(defn click-counter-view [state]
  (-> (layout/->Box 10 [(drawable/->Rectangle 0
                                              0
                                              (if (:has-focus state)
                                                [0 0.8 0.8 1]
                                                [0 0.5 0.5 1]))
                        (drawable/->Text (str (:count state))
                                         (font/create "LiberationSans-Regular.ttf" 15)
                                         [0 0 0 1])])

      (on-mouse-clicked #(update-in % [:count] inc))))



(defn create-click-counter []
  {:count 0
   :handle-keyboard-event handle-click-counter-keyboard-event})

(defn call-view-for-sequence [parent-state view-function key]
  (map-indexed (fn [index child-state]
                 (assoc (view-function child-state)
                   :state-path-part [key index]))
               (key parent-state)))

(defn counter-row-view [state]
  (layout/->Margin 5 0 5 0
                   [(layout/->Box 10 [(drawable/->FilledRoundedRectangle 0 0 10 (if (:child-has-focus state)
                                                                                  [0.8 0.8 0.8 1]
                                                                                  [0.5 0.5 0.5 1]))
                                      (layout/->HorizontalStack (call-view-for-sequence state click-counter-view :counters))])]))

(defn create-counter-row [& counters]
  (-> {:counters (vec counters)}
      (conj (seq-focus-handlers :counters))))

(defn view [state]
  (layout/->VerticalStack (call-view-for-sequence state counter-row-view :counter-rows)))

(defn create-view [& counter-rows]
  (-> {:counter-rows (vec counter-rows)}
      (conj (seq-focus-handlers :counter-rows))))

(defn handle-event [state event]
  (cond
   :default
   state))

(defonce event-queue (atom (event-queue/create)))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (start-view #'view
                                      @event-queue
                                      #'handle-event
                                      (create-view (create-counter-row (create-click-counter) (create-click-counter))
                                                   (create-counter-row (create-click-counter) (create-click-counter))))))))

(event-queue/add-event @event-queue {})

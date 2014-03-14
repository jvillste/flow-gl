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
  (println "apply-keyboard-event-handlers" focus-path-parts)
  (reduce (fn [state focus-path-parts]
            (let [focus-path (apply concat focus-path-parts)]
              (println "focus-path " focus-path "handler " (get-in state (conj (vec focus-path) :handle-keyboard-event)))
              (println "state" state)
              (if-let [keyboard-event-handler (get-in state (conj (vec focus-path) :handle-keyboard-event))]
                (update-in state
                           focus-path
                           keyboard-event-handler
                           event)
                state)))
          state
          (path-prefixes focus-path-parts)))

(defn set-focus-state [state focus-path-parts has-focus]
  (println "set-focus-state" focus-path-parts)
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

(defn start-view [view event-queue event-handler initial-state]
  (let [window (window/create 300
                              400
                              opengl/initialize
                              opengl/resize
                              event-queue)]


    (try
      (loop [state initial-state
             previous-state {}
             previous-focus-path-parts []
             cached-runnables {}]
        (println "state " state)

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
             (let [layout-paths-under-mouse (layout/layout-paths-in-coordinates layout [] (:x event) (:y event))
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
                    live-commands)))))

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
  (println "got event " event)
  (cond (events/key-pressed? event :enter)
        (-> (update-in state [:count] inc))

        :default
        state))

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

(defn view [state]
  (layout/->VerticalStack (call-view-for-sequence state counter-row-view :counter-rows)))


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
                                      {:counter-rows [{:counters [{:count 0 :handle-keyboard-event handle-click-counter-keyboard-event}{:count 0}]}
                                                      {:counters [{:count 0}{:count 0}]}]})))))

(event-queue/add-event @event-queue {})

(ns examples.view-definition
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

(def event-queue (atom (event-queue/create)))

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

(defn apply-keyboard-event-handlers [state event]
  (println "applying " event (:focus-path-parts state))
  (reduce (fn [state focus-path-parts]
            (let [focus-path (apply concat focus-path-parts)]
              (if-let [keyboard-event-handler (get-in state (conj (vec focus-path) :handle-keyboard-event))]
                (if (seq focus-path)
                  (update-in state
                             focus-path
                             keyboard-event-handler
                             event)
                  (keyboard-event-handler state event))
                state)))
          state
          (conj (path-prefixes (:focus-path-parts state))
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

(defn render-state [window cached-runnables state]
  (let [layout (layout/layout ((:view state) state)
                              (window/width window)
                              (window/height window))
        drawing-commands (doall (drawable/drawing-commands layout))
        cached-runnables-atom (atom cached-runnables)
        unused-commands-atom (atom nil)]

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

                     (reset! unused-commands-atom unused-commands)))
    (apply dissoc @cached-runnables-atom @unused-commands-atom)))


(defn set-focus [state focus-path-parts]
  (-> state
      (set-focus-state (:focus-path-parts state) false)
      (set-focus-state focus-path-parts true)
      (assoc :focus-path-parts focus-path-parts)))

(defn handle-event [state width height event]
  (let [layout (layout/layout ((:view state) state)
                              width
                              height)]
    (cond
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
     (do (println "closing")
         nil)

     :default
     (apply-keyboard-event-handlers state
                                    event))))

(defn start-view [event-queue initial-state state-updater]
  (let [window (window/create 300
                              400
                              opengl/initialize
                              opengl/resize
                              event-queue)
        view-state-atom (atom nil)]

    (try
      (reset! view-state-atom (set-focus initial-state (initial-focus-path-parts initial-state)))
      (loop [gpu-state {}]
        (if (:close-requested @view-state-atom)
          (do (println "closing")
              (window/close window))

          (let [new-gpu-state (render-state window gpu-state @view-state-atom)
                event (event-queue/dequeue-event-or-wait event-queue)
                new-state (swap! view-state-atom
                                 (fn [state]
                                   (-> state
                                       (handle-event (window/width window)
                                                     (window/height window)
                                                     event)
                                       (state-updater))))]
            (if new-state
              (recur new-gpu-state)
              (window/close window)))))
      (catch Exception e
        (window/close window)
        (throw e)))))

(defn on-mouse-clicked [layoutable handler]
  (assoc layoutable
    :handle-mouse-event (fn [state event]
                          (if (= :mouse-clicked (:type event))
                            (handler state)
                            state))))

(defn seq-focus-handlers [child-seq-key]
  {:first-focusable-child [child-seq-key 0]
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




(defn append-character [string character]
  (apply str (vec (concat string
                          (str character)))))

(fact (append-character "Foo" \a)
      => "Fooa")

(defn handle-text-editor-event [state event]
  (if (:editing? state)
    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :text (:edited-text state))
         (assoc :editing? false))

     (events/key-pressed? event :back-space)
     (update-in state [:edited-text] (fn [text] (apply str (drop-last text))))

     (and (:character event)
          (= (:type event)
             :key-pressed))
     (update-in state [:edited-text] append-character (:character event))


     :default
     state)

    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :edited-text (:text state))
         (assoc :editing? true))


     :default
     state)))

(defn initial-text-editor-state [text]
  {:text text
   :edited-text ""
   :editing? false
   :has-focus false
   :handle-keyboard-event handle-text-editor-event})

(defn text-editor-view [state]
  (layout/->Box 10 [(drawable/->Rectangle 0
                                          0
                                          (if (:has-focus state)
                                            [0 0.8 0.8 1]
                                            [0 0.5 0.5 1]))
                    (drawable/->Text (if (:editing? state)
                                       (:edited-text state)
                                       (:text state))
                                     (font/create "LiberationSans-Regular.ttf" 15)
                                     (if (:has-focus state)
                                       (if (:editing? state)
                                         [0 0 1 1]
                                         [0 0 0 1])
                                       [0.3 0.3 0.3 1]))]))

(defn initial-button-state [text action-handler]
  {:text text
   :has-focus false
   :handle-keyboard-event (fn [state event]
                            (cond (and (events/key-pressed? event :enter)
                                       (not (:disabled state)))
                                  (do (println "is disabled " (:disabled state) state)
                                      (action-handler)
                                      state)

                                  :default
                                  state))})

(defn button-view [state]
  (layout/->Box 10 [(drawable/->FilledRoundedRectangle 0
                                                       0
                                                       10
                                                       (if (:has-focus state)
                                                         [0 0.8 0.8 1]
                                                         [0 0.5 0.5 1]))
                    (drawable/->Text (:text state)
                                     (font/create "LiberationSans-Regular.ttf" 15)
                                     (if (:disabled state)
                                       [0.5 0.5 0.5 1]
                                       [0 0 0 1]))]))

(defn model-to-state [model state]
  (-> state
      (update-in [:username :text] (:username model))
      (update-in [:full-name :text] (:full-name model))))

(defn state-to-model [state]
  {:username (get-in state [:username :edited-text])
   :full-name (get-in state [:full-name :edited-text])})

(defn registration-view [state]
  (layout/->VerticalStack [(assoc (text-editor-view (:username state))
                             :state-path-part [:username])
                           (assoc (text-editor-view (:full-name state))
                             :state-path-part [:full-name])
                           #_(drawable/->Text (let [model (state-to-model state)]
                                                (if (not (= (:username model)
                                                            (:full-name model)))
                                                  "Passwords differ"
                                                  ""))
                                              (font/create "LiberationSans-Regular.ttf" 25)
                                              [1 1 1 1])
                           (button-view (:submit-button state))]))

(defn update-registration-view-state [state]
  (let [valid? (let [model (state-to-model state)]
                 (and (not (= (:username model) ""))
                      (not (= (:full-name model) ""))))]
    (assoc-in state [:submit-button :disabled] (not valid?))))

(defn initial-registration-view-state []
  {:username (initial-text-editor-state "")
   :full-name (initial-text-editor-state "")
   :submit-button (initial-button-state "Send" (fn [] (println "send")))
   :handle-keyboard-event (fn [state event]
                            (cond (events/key-pressed? event :esc)
                                  (assoc state :close-requested true)

                                  :default
                                  state))
   :view registration-view
   :first-focusable-child [:username]
   :next-focusable-child (fn [this current-focus]
                           (let [children [[:username]  [:full-name] [:submit-button]]
                                 focus-index (first (positions #{current-focus} children))]
                             (get children (inc focus-index))))})

(defmacro with-child-views [view]
  `(binding [view-children (atom [])]
     [~view
      (let [view-children# @view-children]
        (fn [state# model#] (update-focus-container state# model# view-children#)))]))

(defn call-children [key children view]
  )

(def ^:dynamic state-atom)


(defn model-to-view [model]
  #_(apply counters (for [row model]
                      (apply counter-row (for [count row]
                                           (click-counter count))))))




(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (start-view @event-queue
                                      (initial-registration-view-state)
                                      update-registration-view-state
                                      #_(counters (counter-row (click-counter 0) (click-counter 1))
                                                  (counter-row (click-counter 2) (click-counter 3) (click-counter 3))))))))

(event-queue/add-event @event-queue {})

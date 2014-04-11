(ns examples.dynamic-children
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
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

(def ^:dynamic current-event-queue nil)
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

(defn apply-keyboard-event-handlers [state event]
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

(defn render-layout [window cached-runnables layout]
  (let [drawing-commands (doall (drawable/drawing-commands layout))
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
   (apply-keyboard-event-handlers state
                                  event)))

(defn start-view [event-queue initial-state view]
  (let [window (window/create 300
                              400
                              opengl/initialize
                              opengl/resize
                              event-queue)
        [initial-state _] (binding [current-event-queue event-queue]
                            (view initial-state))
        initial-state (set-focus initial-state
                                 (initial-focus-path-parts initial-state))]
    (println "initial state is" initial-state)

    (try

      (loop [gpu-state {}
             state initial-state]
        (if (:close-requested state)
          (window/close window)

          (let [[state visual] (binding [current-event-queue event-queue]
                                 (view state))
                layout (layout/layout visual
                                      (window/width window)
                                      (window/height window))
                new-gpu-state (render-layout window gpu-state layout)
                event (event-queue/dequeue-event-or-wait event-queue)
                new-state (binding [current-event-queue event-queue]
                            (handle-event state
                                          layout
                                          event))]
            (if new-state
              (recur new-gpu-state new-state)
              (window/close window)))))
      (catch Exception e
        (window/close window)
        (throw e)))))

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

(defn text [content & {:keys [color] :or {color [1 1 1 1]}}]
  (drawable/->Text content
                   (font/create "LiberationSans-Regular.ttf" 25)
                   color))

(def ^:dynamic current-state-path [])



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
     (let [new-text (apply str (drop-last (:edited-text state)))]
       (when (:on-change state)
         ((:on-change state) new-text))
       (assoc-in state [:edited-text] new-text))

     (and (:character event)
          (= (:type event)
             :key-pressed))
     (let [new-text (append-character (:edited-text state)
                                      (:character event))]
       (when (:on-change state)
         ((:on-change state) new-text))
       (assoc-in state [:edited-text] new-text))

     :default
     state)

    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :edited-text (:text state))
         (assoc :editing? true))


     :default
     state)))

(def initial-text-editor-state
  {:text ""
   :edited-text ""
   :editing? false
   :has-focus false
   :handle-keyboard-event handle-text-editor-event})

(defn text-editor-view [state]
  [state
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
                                        [0.3 0.3 0.3 1]))])])

(def initial-button-state
  {:text ""
   :has-focus false
   :on-click nil
   :handle-keyboard-event (fn [state event]
                            (cond
                             (events/key-pressed? event :enter)
                             (do (when (:on-click state)
                                   ((:on-click state)))
                                 state)

                             :default
                             state))})

(defn button-view [state]
  [state
   (layout/->Box 10 [(drawable/->FilledRoundedRectangle 0
                                                        0
                                                        10
                                                        (if (:has-focus state)
                                                          [0 0.8 0.8 1]
                                                          [0 0.5 0.5 1]))
                     (drawable/->Text (:text state)
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      (if (:has-focus state)
                                        [0 0 0 1]
                                        [0.3 0.3 0.3 1]))])])

(defn add-child [state path]
  (update-in state [:children] conj path))

(defn reset-children [state]
  (-> state
      (assoc :old-children (or (:children state)
                               []))
      (assoc :children [])))

(defn remove-unused-children [state]
  (let [child-set (set (:children state))]
    (->> (:old-children state)
         (filter (complement child-set))
         (first)
         (apply dissoc state))))

(fact (let [state-1 (-> {}
                        (reset-children)

                        (assoc :1 :foo)
                        (add-child [:1])

                        (assoc :2 :foo)
                        (add-child [:2])

                        (remove-unused-children))]

        state-1 => {:1 :foo
                    :2 :foo
                    :children [[:1]  [:2]]
                    :old-children []}

        (-> state-1
            (reset-children)

            (assoc :2 :foo)
            (add-child [:2])

            (remove-unused-children)) => {:2 :foo
                                          :children [[:2]]
                                          :old-children [[:1] [:2]]}))


(def child-focus-handlers
  {:first-focusable-child (fn [state]
                            (first (:children state)))
   :next-focusable-child (fn [this currently-focused-path-part]
                           (println "next " (:children this) currently-focused-path-part)
                           (let [child-index (first (positions #{currently-focused-path-part} (:children this)))]
                             (let [new-child-index (inc child-index)]
                               (if (= new-child-index
                                      (count (:children this)))
                                 nil
                                 (get (:children this)
                                       new-child-index)))))})


(defn update-or-apply-in [map path function & arguments]
  (if (seq path)
    (apply update-in map path function arguments)
    (apply function map arguments)))

(fact (update-or-apply-in {:foo {:baz :bar}} [] assoc :x :y) => {:foo {:baz :bar}, :x :y}
      (update-or-apply-in {:foo {:baz :bar}} [:foo] assoc :x :y) => {:foo {:baz :bar, :x :y}})

(defn call-view [view-function initial-state state-override state-path-part]
  (let [old-state (or (get-in @current-view-state-atom state-path-part)
                      initial-state)
        new-state (conj old-state state-override)
        [new-state child-visual] (binding [current-state-path (concat current-state-path state-path-part)]
                                   (view-function new-state))]
    (swap! current-view-state-atom assoc-in state-path-part new-state)
    (swap! current-view-state-atom add-child state-path-part)
    (assoc child-visual
      :state-path-part state-path-part)))

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
  (event-queue/add-event current-event-queue
                         (create-apply-to-view-state-event (fn [state]
                                                             (update-or-apply-in state state-path function)))))

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


(defn insert [vector index value]
  (apply conj (vec (take index vector)) value (drop index vector)))

(fact (insert [1 2 3 4] 1 10) => [1 10 2 3 4])

(defn remove [vector index]
  (vec (concat (take index vector) (drop (+ index 1) vector))))

(fact (remove [1 2 3 4] 1) => [1 3 4])

(def-view todo-list-view [state]
  (layout/->VerticalStack (concat (for-all [[index todo-text] (indexed (:todo-texts state))]
                                           (call-view text-editor-view
                                                      initial-text-editor-state
                                                      (let [state-path current-state-path]
                                                        {:text todo-text
                                                         :on-change (fn [new-text]
                                                                      (apply-to-state state-path (fn [state]
                                                                                                   (assoc-in state [:todo-texts index] new-text))))})
                                                      [index]))

                                  [(layout/->HorizontalStack [(call-view button-view
                                                                         initial-button-state
                                                                         (let [state-path current-state-path]
                                                                           {:text "Add new"
                                                                            :on-click #(apply-to-state state-path (fn [state]
                                                                                                                    (update-in state [:todo-texts] conj "New item")))})
                                                                         [:add-new])] )])))



(def initial-todo-list-view-state (conj {:todo-texts ["Do this"
                                                      "Do that"]

                                         :handle-keyboard-event (fn [state event]
                                                                  (cond
                                                                   (events/key-pressed? event :space)
                                                                   (update-in state [:todo-texts] insert (focus-index state) "New item")

                                                                   (events/key-pressed? event :back-space)
                                                                   (update-in state [:todo-texts] remove (focus-index state))

                                                                   (events/key-pressed? event :esc)
                                                                   (assoc state :close-requested true)

                                                                   :default
                                                                   state))}
                                        child-focus-handlers))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (start-view @event-queue
                                      initial-todo-list-view-state
                                      todo-list-view)))))

(event-queue/add-event @event-queue {})

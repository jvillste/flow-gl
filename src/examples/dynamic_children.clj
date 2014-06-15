(ns examples.dynamic-children
  (:require [clojure.core.async :as async]
            [flow-gl.tools.layoutable-inspector :as layoutable-inspector]
            [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events])

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

(defn text [content & {:keys [color size] :or {color [1 1 1 1] size 14}}]
  (drawable/->Text content
                   (font/create "LiberationSans-Regular.ttf" size)
                   color))

(defn layoutable-name [layoutable]
  (-> (clojure.reflect/typename (type layoutable))
      (clojure.string/replace  "flow_gl.gui.layout." "")
      (clojure.string/replace  "flow_gl.gui.drawable." "")))

(defn draw-layoutable [layoutable]
  (layout/->VerticalStack [(if (:state-path-part layoutable)
                             (text (str (:state-path-part layoutable)) :size 12 :color [0.6 0.6 0.6 1.0])
                             (drawable/->Empty 0 0))
                           (text (layoutable-name layoutable))
                           (layout/->Margin 0 0 0 10 [(layout/->VerticalStack (map draw-layoutable (:children layoutable)))])]))


;; todo set new parents to preserved quads
(defn quads-for-layout [layout old-state-paths-to-quads state-path parent next-free-id]
  (let [this-layout-id next-free-id]
    (loop [quads [{:image (buffered-image/create 1 1)
                   :x (:x layout)
                   :y (:y layout)
                   :parent parent}]
           removed-quads []
           children (:children layout)
           next-free-id (inc next-free-id)
           state-paths-to-quads old-state-paths-to-quads]

      (if-let [child (first children)]
        (let [state-path-part (:state-path-part child)
              state-path (if state-path-part
                           (concat state-path state-path-part)
                           state-path)]

          (when state-path-part
            (let [old (second (get state-paths-to-quads state-path))]
              (println "comparing " (= (dissoc old
                                               :children)
                                       (dissoc child
                                               :children))
                       (layoutable-name child)
                       #_(dissoc child :children)
                       #_(dissoc old :children))))

          (if (and state-path-part
                   (= (second (get state-paths-to-quads state-path))
                      child))
            (recur quads
                   removed-quads
                   (rest children)
                   next-free-id
                   state-paths-to-quads)
            (let [state-paths-to-quads (if state-path-part
                                         (assoc state-paths-to-quads state-path [next-free-id child])
                                         state-paths-to-quads)
                  removed-quads (if (and state-path-part (get state-paths-to-quads state-path))
                                  (conj removed-quads (first (get state-paths-to-quads state-path)))
                                  removed-quads)]
              (if (satisfies? drawable/Java2DDrawable child)
                (recur (concat quads
                               [{:image (let [image (buffered-image/create (max 1 (:width child))
                                                                           (max 1 (:height child)))]
                                          (drawable/draw child (buffered-image/get-graphics image))
                                          image)
                                 :x (:x child)
                                 :y (:y child)
                                 :parent this-layout-id}])
                       removed-quads
                       (rest children)
                       (inc next-free-id)
                       state-paths-to-quads)
                (let [[child-quads child-state-path-quads child-removed-quads] (quads-for-layout child old-state-paths-to-quads state-path this-layout-id next-free-id)]
                  (recur (concat quads
                                 child-quads)
                         (concat removed-quads
                                 child-removed-quads)
                         (rest children)
                         (+ next-free-id (count child-quads))
                         (conj state-paths-to-quads child-state-path-quads)))))))

        [quads state-paths-to-quads removed-quads]))))

(deftest quads-for-layout-test
  (is (=  (let [layout (layout/layout (layout/->HorizontalStack [(assoc (layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])
                                                                                                 (assoc (drawable/->Text "Bar"
                                                                                                                         (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                         [1 1 1 1])
                                                                                                   :state-path-part [:bar] )])
                                                                   :state-path-part [:child-states 0])
                                                                 (assoc (layout/->VerticalStack [(drawable/->Text "Foo"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])
                                                                                                 (drawable/->Text "Bar"
                                                                                                                  (font/create "LiberationSans-Regular.ttf" 14)
                                                                                                                  [1 1 1 1])])
                                                                   :state-path-part [:child-states 1])] )
                                      100 100)]
            (println "layout " layout)
            (quads-for-layout (assoc layout :x 0 :y 0)
                              {}
                              []
                              -1
                              0))

          nil)))


(defn render-layout [window gpu-state layout]
  (println "")
  (println "rendering")
  #_(layoutable-inspector/show-layoutable (draw-layoutable layout))
  (window/with-gl window gl
    (opengl/clear gl 0 0 0 1)
    (let [[quads state-paths-to-quads] (quads-for-layout (assoc layout
                                                           :x 0 :y 0)
                                                         (:state-paths-to-quads gpu-state)
                                                         []
                                                         -1
                                                         (:next-free-id (:quad-batch gpu-state)))
          quad-batch (-> (reduce (fn [quad-batch id]
                                   (quad-batch/remove-quad quad-batch gl id))
                                 (:quad-batch gpu-state)
                                 (quad-batch/ids (:quad-batch gpu-state)))
                         (quad-batch/collect-garbage gl)
                         (quad-batch/add-quads gl quads)
                         (quad-batch/draw gl (window/width window) (window/height window)))]
      (assoc gpu-state :quad-batch quad-batch
             :state-paths-to-quads state-paths-to-quads))))

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
                              :profile :gl3
                              :init opengl/initialize
                              :reshape opengl/resize
                              :event-queue event-queue)
        [initial-state _] (binding [current-event-queue event-queue]
                            (view initial-state))
        initial-state (set-focus initial-state
                                 (initial-focus-path-parts initial-state))]
    (try

      (loop [gpu-state (window/with-gl window gl {:quad-batch (quad-batch/create gl)
                                                  :state-paths-to-quads {}})
             state initial-state]
                                        ;(println "state is " state)
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

(def text-editor {:initial-state initial-text-editor-state
                  :view text-editor-view})

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
   (on-mouse-clicked (layout/->Box 10 [(drawable/->FilledRoundedRectangle 0
                                                                          0
                                                                          30
                                                                          (if (:has-focus state)
                                                                            [0 0.8 0.8 1]
                                                                            [0 0.5 0.5 1]))
                                       (drawable/->Text (:text state)
                                                        (font/create "LiberationSans-Regular.ttf" 15)
                                                        (if (:has-focus state)
                                                          [0 0 0 1]
                                                          [0.3 0.3 0.3 1]))])
                     (fn [state]
                       (when (:on-click state)
                         ((:on-click state)))
                       state))])

(def button {:initial-state initial-button-state
             :view button-view})

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


(defn update-or-apply-in [map path function & arguments]
  (if (seq path)
    (apply update-in map path function arguments)
    (apply function map arguments)))

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
  (event-queue/add-event current-event-queue
                         (create-apply-to-view-state-event (fn [state]
                                                             (update-or-apply-in state state-path function)))))

(defmacro apply-to-current-state [[state-parameter & parameters] body]
  `(let [state-path# current-state-path]
     (fn [~@parameters]
       (apply-to-state state-path# (fn [~state-parameter]
                                     ~body)))) )

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

#_(defn insert [vector index value]
    (apply conj (vec (take index vector)) value (drop index vector)))

(fact (insert [1 2 3 4] 1 10) => [1 10 2 3 4])

(defn remove [vector index]
  (vec (concat (take index vector) (drop (+ index 1) vector))))

(fact (remove [1 2 3 4] 1) => [1 3 4])

(def-view todo-list-view [state]
  (layout/->VerticalStack (concat (for-all [[index todo-text] (indexed (:todo-texts state))]
                                           (call-view [:todo-text-editor index]
                                                      text-editor
                                                      {:text todo-text
                                                       :on-change (apply-to-current-state [state new-text]
                                                                                          (assoc-in state [:todo-texts index] new-text))}))

                                  [(layout/->Margin 10 0 0 0
                                                    [(layout/->HorizontalStack [(call-view :new-todo-text
                                                                                           text-editor
                                                                                           {:text (:new-todo-text state)
                                                                                            :on-change (apply-to-current-state [state new-text]
                                                                                                                               (assoc state :new-todo-text new-text))})
                                                                                (layout/->Margin 0 0 0 10
                                                                                                 [(call-view :add-new
                                                                                                             button
                                                                                                             {:text "Add new"
                                                                                                              :on-click (apply-to-current-state [state]
                                                                                                                                                (-> state
                                                                                                                                                    (update-in [:todo-texts] conj (:new-todo-text state))
                                                                                                                                                    (assoc :new-todo-text "")))})])])])])))



(def initial-todo-list-view-state (conj {:todo-texts ["Foo"]
                                         :new-todo-text ""
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

(run-tests)

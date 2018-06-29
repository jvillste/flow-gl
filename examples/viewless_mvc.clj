(ns examples.viewless-mvc
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
  (:use midje.sweet))

(defn start-view [view model event-handler]
  (let [width 300
        height 300
        event-queue (event-queue/create)
        window (window/create width
                              height
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try

      (let [[layoutable view-updater] (view nil model)]

        (loop [state (view-updater nil model)
               model model]

          (let [[layoutable view-updater] (view state model)]

            (window/render window gl
                           (opengl/clear gl 0 0 0 1)
                           (doseq [command (drawable/drawing-commands (layout/layout layoutable
                                                                                     width
                                                                                     height))]
                             (doto (command/create-runner command gl)
                               (command/run gl)
                               (command/delete gl))))

            (let [event (event-queue/dequeue-event-or-wait event-queue)]
              (if (= (:type event)
                     :close-requested)
                (window/close window)
                (let [[state model] (event-handler state model event)]
                  (if (= state :exit)
                    (window/close window)
                    (recur (view-updater state model)
                           model))))))))

      (catch Exception e
        (window/close window)
        (throw e)))))

(defn next-in-focus [in-focus focusables]
  (loop [next-focusables focusables]
    (if (seq next-focusables)
      (if (= in-focus
             (first next-focusables))
        (or (second next-focusables)
            (first focusables))
        (recur (rest next-focusables)))
      nil)))

(defn handle-focus [state model event]
  (cond (events/key-pressed? event :down)
        [(update-in state [:focus]
                    #(next-in-focus % (:children state)))
         model]

        :default (let [old-state (get state (:focus state))
                       new-state ((get (:event-handlers state) (:focus state)) old-state event)]
                   [(assoc state (:focus state) new-state)
                    ((get (:model-updaters state) (:focus state)) model old-state new-state)])))

(defn update-focus-container [old-state model children]
  (let [child-keys (map first children)
        focus-state {:focus (if old-state
                              (if (utils/in? (:focus old-state)
                                             child-keys)
                                (:focus old-state)
                                (first child-keys))
                              (first child-keys))

                     :children (map first children)

                     :event-handlers (reduce (fn [event-handlers [key handler state-initializer model-updater]]
                                               (assoc event-handlers key handler))
                                             {}
                                             children)

                     :model-updaters (reduce (fn [model-updaters [key handler state-initializer model-updater]]
                                               (assoc model-updaters key model-updater))
                                             {}
                                             children)}]

    (-> (reduce (fn [focus-container-state [key handler child-state-initializer model-updater]]
                  (-> focus-container-state
                      (assoc  key (child-state-initializer old-state model))
                      (assoc-in [key :in-focus] (= (:focus focus-container-state) key))))
                focus-state
                children))))

#_(fact (update-focus-container nil {}
                                [[:a (fn [_ _] {:a-state :foo}) :a-handler :a-model-updater]
                                 [:b (fn [_ _] {:b-state :foo}) :b-handler :b-model-updater]])

        => {:a {:a-state :foo
                :in-focus true}
            :b {:b-state :foo
                :in-focus false}
            :children '(:a :b)
            :event-handlers {:a :a-handler, :b :b-handler}
            :focus :a
            :model-updaters {:a :a-model-updater, :b :b-model-updater}})

(def ^:dynamic view-children)

(defn child-view [state key child-spec parameters state-updater model-updater]
  (do (swap! view-children conj [key
                                 (:event-handler child-spec)
                                 (fn [state model]
                                   (state-updater (or (key state)
                                                      (conj (:initial-state child-spec) parameters))
                                                  model))
                                 model-updater])
      ((:view child-spec) (key state))))

(defn bind-from-model-to-state [model-path state-path]
  (fn [state model]
    (assoc-in state state-path (get-in model model-path))))

(defn bind-from-state-to-model [state-path model-path]
  (fn [model old-state new-state]
    (assoc-in model model-path  (get-in new-state state-path))))


(defmacro with-child-views [view]
  `(binding [view-children (atom [])]
     [~view
      (let [view-children# @view-children]
        (fn [state# model#] (update-focus-container state# model# view-children#)))]))

;; APPLICATION

;; local view state
;; given view state used for rendering
;; given view state used for event handling
;; model state

(def initial-counter-state
  {:amount-to-add 0
   :count 0
   :name ""})

(defn handle-counter-event [state event]
  (cond (events/key-pressed? event :enter)
        (update-in state [:count] #(+ % (:amount-to-add state)))

        (events/key-pressed? event :right)
        (update-in state [:amount-to-add] inc)

        :default state))

(defn counter-view [state]
  (drawable/->Text (str (:name state) " : " (:count state) " + " (:amount-to-add state))
                   (font/create "LiberationSans-Regular.ttf" 40)
                   (if (:in-focus state)
                     [1 1 1 1]
                     [0.5 0.5 0.5 1])))

(def counter
  {:initial-state initial-counter-state
   :event-handler handle-counter-event
   :view counter-view})

(defn view [state model]
  (with-child-views
    (layout/->VerticalStack (doall (concat (map-indexed (fn [index model-counter]
                                                          (child-view state
                                                                      (keyword (str "child-" index))
                                                                      counter
                                                                      {:name (:name model-counter)}
                                                                      (bind-from-model-to-state [index :count] [:count])
                                                                      (bind-from-state-to-model [:count] [index :count])))
                                                        model)

                                           [(drawable/->Text (str "Total: " (reduce + (map :count model)))
                                                             (font/create "LiberationSans-Regular.ttf" 40)
                                                             [1 1 1 1])])))))

(defn handle-event [state model event]
  (cond (events/key-pressed? event :esc)
        [:exit model]

        (events/key-pressed? event :space)
        [state
         (conj model {:count 0 :name (str "New Counter " (count model))})]

        (= (:type event)
           :close-requested)
        [:exit model]

        :default (handle-focus state model event)))


(defn start []
  (start-view view
              [{:name "Hello"
                :count 10}
               {:name "World"
                :count 5}]
              handle-event))

;;(start)

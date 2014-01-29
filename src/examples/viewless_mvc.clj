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

(def ^:dynamic current-model-atom)

(defn start-view [view update-state-from-model model event-handler]
  (let [width 300
        height 300
        event-queue (event-queue/create)
        window (window/create width
                              height
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (loop [state (update-state-from-model nil model)
             model model]

        (window/render window gl
                       (opengl/clear gl 0 0 0 1)
                       (doseq [command (drawable/drawing-commands (layout/layout (view state model)
                                                                                 width
                                                                                 height))]
                         (doto (command/create-runner command gl)
                           (command/run gl)
                           (command/delete gl))))

        (let [event (event-queue/dequeue-event-or-wait event-queue)
              model-atom (atom model)]
          (if (= (:type event)
                 :close-requested)
            (window/close window)
            (let [state (binding [current-model-atom model-atom]
                          (event-handler state event))]
              (if (= state :exit)
                (window/close window)
                (recur (update-state-from-model state @model-atom)
                       @model-atom))))))

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

(defn handle-focus [state event]
  (cond (events/key-pressed? event :down)
        (update-in state [:focus]
                   #(next-in-focus % (:children state)))

        #_(events/key-pressed? event :up)
        #_(update-in state [:focus] (fn [focus]
                                      (max 0
                                           (dec focus))))

        :default (update-in state [(:focus state)]
                            (fn [old-state]
                              (let [new-state ((get (:event-handlers state) (:focus state)) old-state event)]
                                (swap! current-model-atom (get (:model-updaters state) (:focus state)) old-state new-state)
                                new-state)))))

(defn update-focus-container [old-state model & children]
  (let [child-keys (map first children)
        focus-state {:focus (if old-state
                              (if (utils/in? (:focus old-state)
                                             child-keys)
                                (:focus old-state)
                                (first child-keys))
                              (first child-keys))

                     :children (map first children)

                     :event-handlers (reduce (fn [event-handlers [key state handler model-updater]]
                                               (assoc event-handlers key handler))
                                             {}
                                             children)

                     :model-updaters (reduce (fn [model-updaters [key state handler model-updater]]
                                               (assoc model-updaters key model-updater))
                                             {}
                                             children)}]

    (reduce (fn [focus-container-state [key child-state-initializer handler model-updater]]
              (assoc focus-container-state key (child-state-initializer old-state model)))
            focus-state
            children)))

(fact (update-focus-container nil
                              :a :a-state :a-handler :a-model-updater
                              :b :b-state :b-handler :b-model-updater)

      => {:a :a-state, :b :b-state, :children '(:a :b),
          :event-handlers {:a :a-handler, :b :b-handler}
          :focus :a
          :model-updaters {:a :a-model-updater, :b :b-model-updater}})

(fact (update-focus-container {:a :a-state, :b :b-state, :children '(:a :b),
                               :event-handlers {:a :a-handler, :b :b-handler}
                               :focus :a
                               :model-updaters {:a :a-model-updater, :b :b-model-updater}}

                              :a :a-state :a-handler :a-model-updater
                              :b :b-state :b-handler :b-model-updater)

      => {:a :a-state, :b :b-state, :children '(:a :b),
          :event-handlers {:a :a-handler, :b :b-handler}
          :focus :a
          :model-updaters {:a :a-model-updater, :b :b-model-updater}})

(defn has-focus? [state child-key]
  (= child-key (:focus state)))

(defn child [key initial-state event-handler bindings]
  [key
   (fn [state model]
     (reduce (fn [state [model-key child-key]]
               (assoc state child-key (model-key model)))
             (or (key state)
                 initial-state)
             bindings))
   event-handler
   (fn [model old-state new-state]
     (reduce (fn [model [model-key child-key]]
               (assoc model model-key (child-key new-state)))
             model
             bindings))])

;; APPLICATION

;; local view state
;; given view state used for rendering
;; given view state used for event handling
;; model state

(def initial-counter-state
  {:amount-to-add 0
   :count 0})

(defn handle-counter-event [state event]
  (cond (events/key-pressed? event :enter)
        (update-in state [:count] #(+ % (:amount-to-add state)))

        (events/key-pressed? event :right)
        (update-in state [:amount-to-add] inc)

        :default state))

(defn counter-view [state name has-focus]
  (drawable/->Text (str name " : " (:count state) " + " (:amount-to-add state))
                   (font/create "LiberationSans-Regular.ttf" 40)
                   (if has-focus
                     [1 1 1 1]
                     [0.5 0.5 0.5 1])))

(defn update-state-from-model [state model]
  (update-focus-container state model

                          (child :foo
                                 initial-counter-state
                                 handle-counter-event
                                 [[:foo :count]])

                          (child :hello
                                 initial-counter-state
                                 handle-counter-event
                                 [[:hello :count]])

                          (child :world
                                 initial-counter-state
                                 handle-counter-event
                                 [[:world :count]])))

(defn view [state model]
  (layout/->VerticalStack [(counter-view (:foo state)
                                         "Foo"
                                         (has-focus? state :foo))
                           (counter-view (:hello state)
                                         "Hello"
                                         (has-focus? state :hello))

                           (counter-view (:world state)
                                         "World"
                                         (has-focus? state :world))]))

(defn handle-event [state event]
  (cond (events/key-pressed? event :esc)
        :exit

        (events/key-pressed? event :space)
        (do (swap! current-model-atom (fn [model]
                                     (-> model
                                         (update-in [:foo] inc)
                                         (update-in [:hello] inc)
                                         (update-in [:world] inc))))
            state)

        (= (:type event)
           :close-requested)
        :exit

        :default (handle-focus state event)))



(defn start []
  (start-view view
              update-state-from-model
              {:foo 23
               :hello 10
               :world 5}
              handle-event))

;;(start)

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
        (println model)

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

(defn update-focus-container [old-state & children]
  (let [children (partition 4 children)
        child-keys (map first children)
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

    (reduce (fn [focus-container-state [key child-state handler model-updater]]
              (assoc focus-container-state key child-state))
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

;; APPLICATION

;; local view state
;; given view state used for rendering
;; given view state used for event handling
;; model state

(defn initialize-counter []
  {:amount-to-add 0
   :count 0})

(defn handle-counter-event [state event]
  (println "counter event " state event)
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
  (update-focus-container state

                          :hello
                          (assoc (or (:hello state)
                                     (initialize-counter))
                            :count (:hello model))
                          handle-counter-event
                          (fn [model old-counter new-counter]
                            (println "updating hello " model " with " new-counter)
                            (assoc model :hello (:count new-counter)))

                          :world
                          (assoc (or (:world state)
                                     (initialize-counter))
                            :count (:world model))
                          handle-counter-event
                          (fn [model old-counter new-counter]
                            (println "updating world " model " with " new-counter)
                            (assoc model :world (:count new-counter)))))

(defn view [state model]
  (layout/->VerticalStack [(counter-view (:hello state)
                                         "Hello"
                                         (has-focus? state :hello))

                           (counter-view (:world state)
                                         "World"
                                         (has-focus? state :world))]))

(defn handle-event [state event]
  (cond (events/key-pressed? event :esc)
        :exit

        (= (:type event)
           :close-requested)
        :exit

        :default (handle-focus state event)))



(defn start []
  (start-view view
              update-state-from-model
              {:hello 10
               :world 5}
              handle-event))

;;(start)

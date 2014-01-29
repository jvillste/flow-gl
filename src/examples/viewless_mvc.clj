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
                            (fn [child]
                              ((get (:event-handlers state) (:focus state)) child event)))))

(defn add-child [state child-key child-state child-event-handler]
  (-> state
      (assoc child-key child-state)
      (update-in [:children] conj child-key)
      (update-in [:event-handlers] assoc child-key child-event-handler)
      (update-in [:focus] (fn [focus] (or focus
                                          child-key)))))

(defn update-focus-container [old-state & children]
  (let [children (partition 3 children)
        child-keys (map first children)
        focus-state {:focus (if old-state
                              (if (utils/in? (:focus old-state)
                                             child-keys)
                                (:focus old-state)
                                (first child-keys))
                              (first child-keys))

                     :children (map first children)

                     :event-handlers (reduce (fn [event-handlers [key state handler]]
                                               (assoc event-handlers key handler))
                                             {}
                                             children)}]

    (reduce (fn [focus-container-state [key child-state handler]]
              (assoc focus-container-state key child-state))
            focus-state
            children)))

(fact (update-focus-container nil
                              :a :a-state :a-handler
                              :b :b-state :b-handler)

      => {:a :a-state
          :b :b-state
          :children '(:a :b)
          :event-handlers {:a :a-handler
                           :b :b-handler}
          :focus :a})

(fact (update-focus-container {:children '(:a :b)
                               :event-handlers {:a :a-handler
                                                :b :b-handler}
                               :focus :a}

                              :a :a-state :a-handler
                              :b :b-state :b-handler)

      => {:a :a-state
          :b :b-state
          :children '(:a :b)
          :event-handlers {:a :a-handler
                           :b :b-handler}
          :focus :a})

(defn has-focus? [state child-key]
  (= child-key (:focus state)))


;; APPLICATION

;; local view state
;; given view state used for rendering
;; given view state used for event handling
;; model state

(defn initialize-counter [old-state]
  {:amount-to-add (or (:amount-to-add old-state)
                      0)})

(defn handle-counter-event [model-path state event]
  (cond (events/key-pressed? event :enter)
        (do (swap! current-model-atom update-in model-path #(+ % (:amount-to-add state)))
            state)

        (events/key-pressed? event :right)
        (update-in state [:amount-to-add] inc)

        :default state))

(defn counter-view [state name count has-focus]
  (drawable/->Text (str name " : " count " + " (:amount-to-add state))
                   (font/create "LiberationSans-Regular.ttf" 40)
                   (if has-focus
                     [1 1 1 1]
                     [0.5 0.5 0.5 1])))

(defn update-state-from-model [state model]
  (update-focus-container state

                          :hello
                          (initialize-counter (:hello state))
                          (partial handle-counter-event [:hello])

                          :world
                          (initialize-counter (:world state))
                          (partial handle-counter-event [:world])))

(defn view [state model]
  (layout/->VerticalStack [(counter-view (:hello state)
                                         "Hello"
                                         (:hello model)
                                         (has-focus? state :hello))

                           (counter-view (:world state)
                                         "World"
                                         (:world model)
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
              {:hello 0
               :world 0}
              handle-event))

;;(start)

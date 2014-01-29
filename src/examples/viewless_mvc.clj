(ns examples.viewless-mvc
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events])
            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window])))

(defn start-view [view state event-handler]
  (let [width 300
        height 300
        event-queue (event-queue/create)
        window (window/create width
                              height
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (loop [state state]

        (window/render window gl
                       (opengl/clear gl 0 0 0 1)
                       (doseq [command (drawable/drawing-commands (layout/layout (view state)
                                                                                 width
                                                                                 height))]
                         (doto (command/create-runner command gl)
                           (command/run gl)
                           (command/delete gl))))

        (let [event (event-queue/dequeue-event-or-wait event-queue)]
          (if (= (:type event)
                 :close-requested)
            (window/close window)
            (let [state (event-handler state event)]
              (if (= state :exit)
                (window/close window)
                (recur state))))))

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

(defn create-focus-container []
  {:focus nil
   :children []
   :event-handlers {}})

(defn has-focus? [state child-key]
  (= child-key (:focus state)))


;; APPLICATION

(defn create-counter [name count]
  {:count 0
   :name name})

(defn handle-counter-event [state event]
  (cond (events/key-pressed? event :enter)
        (update-in state [:count] inc)

        :default state))

(defn counter-view [state has-focus]
  (drawable/->Text (str (:name state) " : " (:count state))
                   (font/create "LiberationSans-Regular.ttf" 40)
                   (if has-focus
                     [1 1 1 1]
                     [0.5 0.5 0.5 1])))

(def model-atom (atom {:hello 0
                       :world 0}))

(defn model-to-view-state [model]
  (-> (create-focus-container)
      (add-child :hello
                 (create-counter "Hello" (:hello model))
                 handle-counter-event)

      (add-child :world
                 (create-counter "World" (:world model))
                 handle-counter-event)))

(defn view [state]
  (layout/->VerticalStack [(counter-view (:hello state)
                                         (has-focus? state :hello))
                           (counter-view (:world state)
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
              (model-to-view-state model)
              handle-event))

;;(start)

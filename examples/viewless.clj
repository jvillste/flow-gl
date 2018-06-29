(ns examples.viewless
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

(defn handle-focus [state event]
  (cond (events/key-pressed? event :down)
        (update-in state [:focus] (fn [focus]
                                    (min (- (count (:focusable-children state))
                                            1)
                                         (inc focus))))

        (events/key-pressed? event :up)
        (update-in state [:focus] (fn [focus]
                                    (max 0
                                         (dec focus))))

        :default (update-in state [:focusable-children] (fn [counters]
                                                          (map-indexed (fn [index child]
                                                                         (if (= index (:focus state))
                                                                           ((get (:event-handlers state) index) child event)
                                                                           child))
                                                                       (:focusable-children state))))))

(defn add-focusable-child [state child-state child-event-handler]
  (-> state
      (update-in [:focusable-children] conj child-state )
      (update-in [:event-handlers] conj child-event-handler)))

(defn create-focus-container []
  {:focus 0
   :focusable-children []
   :event-handlers []})

(defn create-counter [name]
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


(defn view [state]
  (layout/->VerticalStack (map-indexed (fn [index counter]
                                         (counter-view counter
                                                       (= index (:focus state))))
                                       (:focusable-children state))))

(defn handle-event [state event]
  (println event)
  (cond (events/key-pressed? event :esc)
        :exit

        (= (:type event)
           :close-requested)
        :exit

        :default (handle-focus state event)))

(defn start []
  (start-view view
              (-> (create-focus-container)
                  (add-focusable-child (create-counter "Hello")
                                       handle-counter-event)

                  (add-focusable-child (create-counter "World")
                                       handle-counter-event))
              handle-event))

;;(start)

(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view]
                         [events :as events]
                         [event-queue :as event-queue])
            (flow-gl.graphics [font :as font])
            (flow-gl.dataflow [triple-dataflow :as triple-dataflow])))


(defn hello [state text]
  (drawable/->Text (str text " " (:counter state))
                   (font/create "LiberationSans-Regular.ttf" 40)
                   [1 1 1 1]))

(defn initialize-hello [state]
  (assoc state :countter 0))

(defn hello-box [state]
  (layout/->VerticalStack [(layout/->Box  10
                                          (drawable/->FilledRoundedRectangle 0 0 15 [0 0 0.9 1])
                                          (view/call-child-view initialize-hello hello "Hello world"))]))

(defn initialize-hello-box [state state-atom]
  (assoc state :count 1))

(defn handle-hello-box-event [state event]
  (cond (events/key-pressed? event :enter)
        (update-in state :count inc)

        (events/key-pressed? event :esc)
        (do (event-queue/add-event (application/event-queue-from-view-state state)
                                (events/create-close-requested-event))
            state)

        :default state))

(defn start []
  (application/create-window hello-box
                             :handle-event handle-hello-box-event))

(comment
  (start))

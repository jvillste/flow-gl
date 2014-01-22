(ns examples.hello-worlds
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view]
                         [events :as events]
                         [event-queue :as event-queue])
            (flow-gl.graphics [font :as font])
            (flow-gl.dataflow [triple-dataflow :as triple-dataflow])))


(defn hello [state text count]
  (drawable/->Text (str text " " count)
                   (font/create "LiberationSans-Regular.ttf" 40)
                   [1 1 1 1]))

(defn initialize-hello [state]
  (assoc state :counter 0))

(defn hello-box [state]
  (layout/->VerticalStack [(layout/->Box  10
                                          (drawable/->FilledRoundedRectangle 0 0 15 [0 0 0.9 1])
                                          (view/call-child-view hello initialize-hello "Hello world" (:counter state)))]))

(defn initialize-hello-box [state]
  (assoc state :counter 0))

(defn handle-hello-box-event [state event]
  (cond (events/key-pressed? event :enter)
        (do (println "couter is " (:counter state))
            (update-in state [:counter] inc))

        (events/key-pressed? event :esc)
        (do (event-queue/add-event (application/event-queue-from-view-state state)
                                   (events/create-close-requested-event))
            state)

        :default state))

(defn start []
  (application/create-window hello-box
                             :root-view-initializer initialize-hello-box
                             :handle-event handle-hello-box-event))

(comment
  (start))

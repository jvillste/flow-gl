(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view]
                         [events :as events]
                         [event-queue :as event-queue])
            (flow-gl.graphics [font :as font])
            (flow-gl.dataflow [triple-dataflow :as triple-dataflow])))


(defn hello [count]
  (drawable/->Text (str "Hello World " count)
                   (font/create "LiberationSans-Regular.ttf" 40)
                   [1 1 1 1]))

(defn hello-box [state]
  (layout/->VerticalStack [(layout/->Box  10
                                          [(drawable/->FilledRoundedRectangle 0 0 15 [0 0 0.9 1])
                                           (hello (:counter state))])]))

(defn initialize-hello-box [state]
  (assoc state :counter 0))

(defn handle-hello-box-event [state event]
  (events/on-key-apply state event
                       :enter :counter inc))

(defn start []
  (application/create-window hello-box
                             :root-view-initializer initialize-hello-box
                             :handle-event handle-hello-box-event))

(comment
  (start)
)

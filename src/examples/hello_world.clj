(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view]
                         [events :as events])
            (flow-gl.graphics [font :as font])
            (flow-gl.dataflow [triple-dataflow :as triple-dataflow])))


(defn hello [state text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 40)
                   [1 1 1 1]))

(defn hello-box [state]
  (layout/->VerticalStack [(layout/->Box  10
                                          (drawable/->FilledRoundedRectangle 0 0 15 [0 0 0.9 1])
                                          (view/call-child-view hello "Hello world"))]))

(defn initialize [state state-atom]
  (assoc state :count 1))

(defn handle-event [state event]
  (cond (events/key-pressed? event :enter)
        (update-in state :count inc)

        :default (events/close-on-esc state event)))

(defn start []
  (application/create-window hello-box
                             :handle-event handle-event))

(comment
  (start))

(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view]
                         [events :as events])
            (flow-gl.graphics [font :as font])
            (flow-gl.dataflow [triple-dataflow :as triple-dataflow])))


(view/defview hello [state text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 40)
                   [1 1 1 1]))

(view/defview hello-box [state]
  (layout/->VerticalStack [(layout/->Box  10
                                          (drawable/->FilledRoundedRectangle 0 0 15 [0 0 0.9 1])
                                          (view/call-child-view state [:hello] hello "Hello world" ))]))

(defn handle-event [state event]
  (println "got event " event)

  (cond (events/key-pressed? event :enter)
        (do (println "got enter")
            state)
        
        :default state))

(defn start []
  (application/create-window hello-box
                             :handle-event handle-event))

(comment
  (start))

(ns examples.drag-and-drop
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-gui :as quad-gui]
                         [events :as events])

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(quad-gui/def-view view [state]
  (layout/->VerticalStack [(assoc (layout/->Box 10
                                                [(drawable/->Rectangle 0 0 [0 1 1 1])
                                                 (layout/->FixedSize (:width state)
                                                                     (:height state)
                                                                     [(drawable/->Text "Bla bla bla bla bla"
                                                                                       (font/create "LiberationSans-Regular.ttf" 15)
                                                                                       [1 1 1 1])])])
                             :on-drag (fn [state x y]
                                        (-> state
                                            (update-in [:width] + x)
                                            (update-in [:height] + y))))]))

(defn create [state-path event-channel control-channel]
  {:width 100
   :height 100
   :handle-keyboard-event (fn [state event]
                            (events/on-key state event
                                           :esc (do (quad-gui/request-close event-channel)
                                                    state)))})


(defn start []
  (quad-gui/start-view {:constructor create
                        :view view}))


(run-tests)

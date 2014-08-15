(ns flow-gl.gui.components.button
  (:require [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [events :as events]
                         [view :as view])

            (flow-gl.graphics [font :as font]))
  
  (:use midje.sweet))

#_(def initial-button-state
  {:text ""
   :has-focus false
   :on-click nil
   :handle-keyboard-event (fn [state event]
                            (cond
                             (events/key-pressed? event :enter)
                             (do (when (:on-click state)
                                   ((:on-click state)))
                                 state)

                             :default
                             state))})

#_(defn button-view [state]
  [state
   (view/on-mouse-clicked (layout/->Box 10 [(drawable/->FilledRoundedRectangle 0
                                                                          0
                                                                          10
                                                                          (if (:has-focus state)
                                                                            [0 0.8 0.8 1]
                                                                            [0 0.5 0.5 1]))
                                       (drawable/->Text (:text state)
                                                        (font/create "LiberationSans-Regular.ttf" 15)
                                                        (if (:has-focus state)
                                                          [0 0 0 1]
                                                          [0.3 0.3 0.3 1]))])
                     (fn [state]
                       (when (:on-click state)
                         ((:on-click state)))
                       state))])

#_(def button {:initial-state initial-button-state
             :view button-view})

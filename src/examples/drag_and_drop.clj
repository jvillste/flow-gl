(ns examples.drag-and-drop
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-gui :as quad-gui]
                         [events :as events])

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(quad-gui/def-view view [state]
  (let [font (font/create "LiberationSans-Regular.ttf" 15)
        color [1 1 1 1]]
    (layout/->Box 10
                  [(drawable/->Empty 0 0)
                   (layout/->Flow (repeat 10 (drawable/->Text "Blaaaaaaaaa " font color)))])))

(defn create [state-path event-channel control-channel]
  {:handle-keyboard-event (fn [state event]
                            (events/on-key state event
                                           :esc (do (quad-gui/request-close event-channel)
                                                    state)))})

(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (quad-gui/start-view {:constructor create
                        :view view})
  (flow-gl.debug/write-log))


(run-tests)

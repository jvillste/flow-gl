(ns examples.drag-and-drop
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-gui :as quad-gui]
                         [events :as events])

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(quad-gui/def-view view [state]
  (layout/->Flow (repeat 10
                         (drawable/->Text "Blaaaaaaaaa "
                                          (font/create "LiberationSans-Regular.ttf" 15)
                                          [1 1 1 1]))))

(defn create [state-path event-channel control-channel]
  {})

(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (quad-gui/start-view {:constructor create
                        :view view})
  (flow-gl.debug/write-timed-log))


(run-tests)

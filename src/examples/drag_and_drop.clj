(ns examples.drag-and-drop
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-gui :as quad-gui]
                         [events :as events])

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(quad-gui/def-view view [state]
  (do (println "view")
      (assoc (layout/->Box 10
                        [(drawable/->Rectangle 0 0 [0 1 1 1])
                         (drawable/->Text "Bla bla bla bla bla"
                                          (font/create "LiberationSans-Regular.ttf" 15)
                                          [1 1 1 1])])
     :on-drag (fn [state x y]
                (-> state
                    (update-in [:width] + x)
                    (update-in [:height] + y))))))

(defn create [state-path event-channel control-channel]
  {:width 100
   :height 100
   :handle-keyboard-event (fn [state event]
                            (events/on-key state event
                                           :esc (do (quad-gui/request-close event-channel)
                                                    state)))})


#_(flow-gl.debug/set-active-channels :all)

(defn start []
  #_(flow-gl.debug/reset-log)
  (quad-gui/start-view {:constructor create
                        :view view})
  #_(flow-gl.debug/write-log))


(run-tests)

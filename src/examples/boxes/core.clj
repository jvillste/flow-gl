(ns examples.boxes.core
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow])
            [examples.boxes.box :as box]))

(defn view []
  #_(view/init-and-call :box1 box/view)

  (layout/->Stack [(view/init-and-call :box1 box/view)
                   (view/init-and-call :box2 box/view)]))

(defn handle-event [state view event]
  (println "event " event)
  (box/handle-event state
                    (concat view [:box1])
                    event))

(defn initialize [state state-atom]
  state)

(defn start []
  (application/start view
                     :handle-event handle-event
                     :initialize initialize
                     :framerate 20))

(defn refresh []
  (when @application/state-atom-atom
    (swap! @application/state-atom-atom
           view/set-view
           view)))

(refresh)

(defn start-async []
  (.start (Thread. start)))

(comment
  (start-async)
(start)
  )

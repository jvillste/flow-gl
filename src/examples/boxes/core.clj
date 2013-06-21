(ns examples.boxes.core
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application]
                         [focus :as focus])

            (flow-gl [dataflow :as dataflow])
            [examples.boxes.box :as box])
  (:use clojure.test))


(defn view []
  (focus/set-focusable-children :box1 box/handle-event
                                :box2 box/handle-event
                                :box3 box/handle-event)

  (layout/->Stack [(view/init-and-call :box1 box/view)
                   (view/init-and-call :box2 box/view)
                   (view/init-and-call :box3 box/view)]))



(defn handle-event [state event]
  (cond (input/key-pressed? event input/tab)
        (focus/move-focus state)

        (input/key-pressed? event input/esc)
        (do (application/request-close)
            state)

        :default
        (focus/pass-event-to-focused-child state event)))


(defn start []
  (application/start view
                     :handle-event handle-event
                     :framerate 60))


(comment
  (.start (Thread. start))
  (start)
  )

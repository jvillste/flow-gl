(ns examples.remote-editor
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [animation :as animation]
                         [events :as events]
                         [application :as application]
                         [focus :as focus])

            (flow-gl [dataflow :as dataflow]))
  (:use clojure.test))



(defn box-view [width height]
  (dataflow/initialize :checked false)

  (drawable/->FilledRoundedRectangle width
                                     height
                                     5
                                     (if (dataflow/get-value-or-nil :checked)
                                       [0.8 0.2 0 1]
                                       [0.2 0.8 0 1])))

(defn box-handle-event [state event]
  (events/on-key-apply state event input/enter :checked not))


(defn view []
  (layout/->VerticalStack [(view/init-and-call :box (partial box-view 20 20))]))


(defn handle-event [state event]
  (cond (input/key-pressed? event input/esc)
        (do (application/request-close)
            state)

        :default
        (binding [dataflow/current-path (dataflow/path dataflow/current-path :box)]
          (box-handle-event state event))))

(defn start []
  (application/start view
                     :handle-event handle-event
                     :framerate 60))


(comment
  (.start (Thread. start))
(start)
  )

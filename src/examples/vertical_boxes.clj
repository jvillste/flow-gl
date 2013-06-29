(ns examples.vertical-boxes
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


(defn box-view []
  (dataflow/initialize :checked false)
  (let [size 80]
    (drawable/->FilledRoundedRectangle size
                                       size
                                       20
                                       (if (dataflow/get-value-or-nil :checked)
                                         [1 0 0 1]
                                         [0 1 0 1]))))


(defn box-handle-event [state event]
  (events/on-key-apply state event input/space :checked not))



(defn view []
  (apply focus/set-focusable-children (apply concat (for [index (range 1 4)]
                                                      [(keyword (str "box" index)) box-handle-event])))

  (layout/->VerticalStack (vec (for [index (range 1 4)]
                                 (layout/->Box 5 (drawable/->Empty) (view/init-and-call (keyword (str "box" index)) box-view))))))



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

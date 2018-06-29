(ns examples.boxes.box
  (:require (flow-gl.gui [input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [events :as events]
                         [animation :as animation])
            (flow-gl [dataflow :as dataflow])))

(comment 
  (defn view []

    (dataflow/initialize
     :x 0
     :y 0)

    (animation/initialize-animation :focus)

    (let [max-size 80
          duration 0.3
          size (if-let [time (animation/animation-time :focus duration)]
                 (* max-size
                    (animation/sin-wave 1 0.8 (/ duration 2) time))
                 max-size)]

      (layout/->Absolute [(assoc (drawable/->FilledRoundedRectangle size size 20 (if (dataflow/get-value-or-nil :has-focus)
                                                                                   [1 0 0 1]
                                                                                   [0 1 0 1]))
                            :x (+ (* max-size (dataflow/get-value :x))
                                  (/ (- max-size size) 2))
                            :y (+ (* max-size (dataflow/get-value :y))
                                  (/ (- max-size size) 2)))])))


  (defn handle-event [state event]
    (if (= (:type event)
           :focus-gained)

      (animation/start-animation state :focus)

      (events/on-key-apply state event
                           input/down :y inc
                           input/up :y dec
                           input/left :x dec
                           input/right :x inc
                           input/space :animate not)))
  )

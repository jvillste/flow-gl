(ns examples.boxes.box
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [events :as events])
            (flow-gl [dataflow :as dataflow])))

(defn view []
  (dataflow/initialize
   :x 0
   :y 0
   :animate true
   :animation-state 0)

  (let [time-factor (-> (if (dataflow/get-value :animate)
                          (dataflow/get-global-value :time)
                          (System/nanoTime))
                        (* 4)
                        (mod (* 4 1e9))
                        (/ (* 4 1e9))
                        (* 2 Math/PI)
                        (Math/sin)
                        (+ 1)
                        (/ 2)
                        (* 0.2)
                        (+ 0.8))
        max-size 80
        half-size (/ max-size 2)
        size (+ half-size (* time-factor half-size))]
    (layout/->Absolute [(assoc (drawable/->FilledRoundedRectangle size size 20 (if (dataflow/get-value-or-nil :has-focus)
                                                                                 [1 0 0 1]
                                                                                 [0 1 0 1]))
                          :x (+ (* max-size (dataflow/get-value :x))
                                (/ (- max-size size) 2))
                          :y (+ (* max-size (dataflow/get-value :y))
                                (/ (- max-size size) 2)))])))


(defn handle-event [state event]
  (events/on-key-apply state event
                       input/down :y inc
                       input/up :y dec
                       input/left :x dec
                       input/right :x inc
                       input/space :animate not))

(ns examples.boxes.box
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])
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
                        (/ 2))
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

(defn on-key-apply-one [state event key path function]
  `((input/key-pressed? ~event ~key)
    (dataflow/apply-to-value ~state ~path ~function)))

(defmacro on-key-apply [state event & specs]
  (let [specs (partition 3 specs)]
    `(cond ~@(mapcat (fn [[key path function]]
                       (on-key-apply-one state event key path function))
                     specs )
           :default ~state)))

(defn p [prefix key]
  (concat prefix [key]))

(defn handle-event [state box event]
  (on-key-apply state event
                input/down (p box :y) inc
                input/up (p box :y) dec
                input/left (p box :x) dec
                input/right (p box :x) inc
                input/space (p box :animate) not))

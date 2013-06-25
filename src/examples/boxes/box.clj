(ns examples.boxes.box
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [events :as events]
                         [animation :as animation])
            (flow-gl [dataflow :as dataflow])))

(defn initialize-animation [animation-keyword]
  (let [animation-name (name animation-keyword)]
        (dataflow/initialize
         (symbol (str "animate-" animation-name)) false
         (symbol (str animation-name "-animation-started")) 0)))

(defn view []

  (dataflow/initialize
   :x 0
   :y 0)

  (dataflow/initialize
   :animate-focus false
   :focus-animation-started 0)

  ;;(initialize-animation :focus)

  (let [max-size 80
        size (if (dataflow/get-value :animate-focus)
               (let [time (dataflow/get-global-value :time)
                     start-time (dataflow/get-value :focus-animation-started)
                     animation-duration 0.3]
                 (if (< (- time start-time)
                        (* animation-duration 1e9))
                   (* max-size
                      (animation/sin-wave 1 0.8 (/ animation-duration 2) start-time time))
                   (do (dataflow/define :animate-focus false)
                       max-size)))

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

    (-> state
        (dataflow/define-to :animate-focus true)
        (dataflow/define-to :focus-animation-started (dataflow/get-global-value-from state :time)))

    (events/on-key-apply state event
                         input/down :y inc
                         input/up :y dec
                         input/left :x dec
                         input/right :x inc
                         input/space :animate not)))

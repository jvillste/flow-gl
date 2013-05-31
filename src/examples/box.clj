(ns examples.box
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow])))

(defn view []
  (let [time-factor (-> (if (dataflow/get-global-value :animate)
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
    (layout/->Absolute [(assoc (drawable/->FilledRoundedRectangle size size 20 [1 1 0 1])
                          :x (+ (* max-size (dataflow/get-global-value :x))
                                (/ (- max-size size) 2))
                          :y (+ (* max-size (dataflow/get-global-value :y))
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

(defn handle-event [state view event]
  (on-key-apply state event
                input/down :y inc
                input/up :y dec
                input/left :x dec
                input/right :x inc
                input/space :animate not)

  #_(cond (input/key-pressed? event input/down)
          (dataflow/apply-to-value state :y inc)

          (input/key-pressed? event input/up)
          (dataflow/apply-to-value state :y dec)

          (input/key-pressed? event input/right)
          (dataflow/apply-to-value state :x inc)

          (input/key-pressed? event input/left)
          (dataflow/apply-to-value state :x dec)

          (input/key-pressed? event input/space)
          (dataflow/apply-to-value state :animate not)

          :default state))

(defn initialize [state state-atom]
  (dataflow/define-to state
    :x 0
    :y 0
    :animate true
    :animation-state 0))

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
(ns examples.focus-highlight
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


(defn focus-box-view [target-path-value]

  (let [target-layout (-> target-path-value
                          dataflow/get-global-value
                          view/element-path-to-layout-path
                          dataflow/get-global-value)

        parent-layout (-> dataflow/current-path
                          drop-last
                          view/element-path-to-layout-path
                          dataflow/get-global-value)
        margin 5]

    (layout/->Absolute [(-> (drawable/->FilledRoundedRectangle (+ (* 2 margin) (:width target-layout))
                                                               (+ (* 2 margin) (:height target-layout))
                                                               10
                                                               [0 0 1 0.3])
                            (assoc :x (- (:global-x target-layout)
                                         (:global-x parent-layout)
                                         margin)

                                   :y (- (:global-y target-layout)
                                         (:global-y parent-layout)
                                         margin)))])))

(defn box-view [width height]
  (dataflow/initialize :checked false)

  (let [size 80]
    (drawable/->FilledRoundedRectangle width
                                       height
                                       5
                                       (if (dataflow/get-value-or-nil :checked)
                                         [0.8 0.2 0 1]
                                         [0.2 0.8 0 1]))))

(defn box-handle-event [state event]
  (events/on-key-apply state event input/enter :checked not))


(defn list-view []
  (apply focus/set-focusable-children (apply concat (for [index (range 1 4)]
                                                      [(keyword (str "box" index)) box-handle-event])))

  (layout/->VerticalStack (vec (for [index (range 1 4)]
                                 (layout/->Box 5
                                               (drawable/->Empty 0 0)
                                               (view/init-and-call (keyword (str "box" index)) (partial box-view (+ 20 (rand-int 100)) (+ 20 (rand-int 100)))))))))


(defn list-view-handle-event [state event]
  (cond (input/key-pressed? event input/tab)
        (focus/move-focus state)

        :default
        (focus/pass-event-to-focused-child state event)))


(defn view []
  (dataflow/define :highlighted-view-path (partial focus/view-path-in-focus dataflow/current-path))

  (focus/set-focusable-children :list-view-1 list-view-handle-event
                                :list-view-2 list-view-handle-event)

  (layout/->Stack [(drawable/->Rectangle (dataflow/get-global-value :width)
                                         (dataflow/get-global-value :height)
                                         [1 1 1 1])
                   (layout/->Superimpose [(layout/->HorizontalStack [(view/init-and-call :list-view-1 list-view)
                                                                     (view/init-and-call :list-view-2 list-view)])
                                          (view/init-and-call :focus-highlight (partial focus-box-view (dataflow/absolute-path :highlighted-view-path)))])]))


(defn handle-event [state event]
  (cond (input/key-pressed? event input/space)
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

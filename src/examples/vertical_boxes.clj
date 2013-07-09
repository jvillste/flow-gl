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


(defn focus-box-view [view-to-follow]

  (let [layout-path (-> view-to-follow
                        dataflow/get-global-value
                        view/element-path-to-layout-path)
        layout-property (fn [key] (-> (dataflow/path layout-path key)
                                      dataflow/get-global-value))]

    (layout/->Absolute [(-> (drawable/->FilledRoundedRectangle (layout-property :width)
                                                               (layout-property :height)
                                                               10
                                                               [0 0 1 0.3])
                            (assoc :x (layout-property :x)
                                   :y (layout-property :y)))])))

(defn box-view [width height]
  (dataflow/initialize :checked false)

  (let [size 80]
    (drawable/->FilledRoundedRectangle width
                                       height
                                       5
                                       (if (dataflow/get-value-or-nil :checked)
                                         [1 0 0 1]
                                         [0 1 0 1]))
    ))

(defn box-handle-event [state event]
  (events/on-key-apply state event input/space :checked not))

(defn view []
  (apply focus/set-focusable-children (apply concat (for [index (range 1 4)]
                                                      [(keyword (str "box" index)) box-handle-event])))

  (let [child-in-focus-path (dataflow/absolute-path :child-in-focus)
        parent-path dataflow/current-path]
    (dataflow/define :highlighted-view (fn [] (->> (dataflow/get-global-value child-in-focus-path)
                                                   (dataflow/path parent-path)))))


  (layout/->Stack [(drawable/->Rectangle (dataflow/get-global-value :width)
                                         (dataflow/get-global-value :height)
                                         [1 1 1 1])

                   (layout/->VerticalStack (vec (for [index (range 1 4)]
                                                  (layout/->Box 5
                                                                (drawable/->Empty)
                                                                (view/init-and-call (keyword (str "box" index)) (partial box-view (+ 20 (rand-int 100)) (+ 20 (rand-int 100))))))))

                   (view/init-and-call :focus-highlight (partial focus-box-view (dataflow/absolute-path :highlighted-view)))]))



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

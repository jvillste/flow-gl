(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view])
            (flow-gl.dataflow [triple-dataflow :as triple-dataflow])))

(defn start []
  (application/start-window (view/view [state]
                                       (let [globals (triple-dataflow/switch-entity state :globals)]
                                         (layout/->VerticalStack [(drawable/->Rectangle (- (:width globals)
                                                                                           20) 100 [0 0 1 1])
                                                                  (drawable/->Rectangle 10 10 [0 1 0 1])]))))
  ;;(application/start (view/view [state] (drawable/->Rectangle 100 100 [0 1 1 1])))
  )

(defn start-async []
  (.start (Thread. start)))

(comment
  (start))

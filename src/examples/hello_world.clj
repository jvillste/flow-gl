(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view])
            (flow-gl.graphics [font :as font])
            (flow-gl.dataflow [triple-dataflow :as triple-dataflow])))

(defn start []
  (application/start-window (view/view [state]
                                       (let [globals (triple-dataflow/switch-entity state :globals)]
                                         (layout/->VerticalStack [(layout/->Box  10
                                                                                 (drawable/->FilledRoundedRectangle 0 0 15 [0 0 0.9 1])
                                                                                 (drawable/->Text "Hello world"
                                                                                                  (font/create "LiberationSans-Regular.ttf" 40)
                                                                                                  [1 1 1 1]))])))))

(comment
  (start))

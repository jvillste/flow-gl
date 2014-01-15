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
                                         (layout/->Box  10
                                                        (drawable/->Rectangle (- (:width globals)
                                                                                 20) 100 [0 0 1 1])
                                                        (drawable/->Text "Hello world"
                                                                         (font/create "LiberationSans-Regular.ttf" 20)
                                                                         [1 1  1 1]))))))

(comment
  (start))

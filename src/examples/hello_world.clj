(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [application :as application]
                         [view :as view])))
(defn start []
   (application/start (view/view [state] (layout/->VerticalStack [(drawable/->Rectangle 100 100 [0 0 1 1])
                                                                  (drawable/->Rectangle 10 10 [0 1 0 1])])))
  ;;(application/start (view/view [state] (drawable/->Rectangle 100 100 [0 1 1 1])))
  )

(comment
  (start))

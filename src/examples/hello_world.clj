(ns examples.hello-world
  (:require (flow-gl.gui [drawable :as drawable]
                         [application :as application])))

(defn start []
  (application/start (fn [] (drawable/->Rectangle 100 100 [0 0 1 1]))))

(comment
(start))

(start)

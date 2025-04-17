(ns fungl.examples.clip
  (:require
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.component.text-area :as text-area]))

;; (defn root []

;;   {:node (visuals/clip {:node (text-area/text "fo")
;;                         :x 0
;;                         :y -10})
;;    :x -100
;;    :y 100})

(defn app []
  (text-area/text "hello world!"))

(application/def-start app)

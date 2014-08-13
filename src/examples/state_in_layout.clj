(ns examples.state-in-layout
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
                         #_[events :as events]
                         #_[layoutable :as layoutable])

            #_(flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(quad-gui/def-view foo-view [state]
  (drawable/->Rectangle (:width state) 10 (:color state)))

(def foo {:view foo-view
          :constructor (fn [path events control] {:width 20})})

(quad-gui/def-view view [state]
  (layouts/->SizeDependent (fn [available-width available-height]
                             {:width 10 :height 10})
                           (fn [state requested-width requested-height]
                             (quad-gui/call-view :foo
                                                 foo
                                                 {:color (if (> requested-width requested-height)
                                                           [1 0 0 1]
                                                           [0 0 1 1])}))))



(defn start []
  (quad-gui/start-view {:view view
                        :constructor (fn [path events control]
                                       {:width 20})}))

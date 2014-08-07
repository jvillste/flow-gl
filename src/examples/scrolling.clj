(ns examples.scrolling
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-gui :as quad-gui]
                         [events :as events])

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(quad-gui/def-view scroll-bar-view [state]
  (let [width 30
        margin 5]
    (layout/->SizeDependent (fn [available-width available-height]
                              {:width width :height 0})
                            (fn [requested-width requested-height]
                              (layout/->Superimpose [(drawable/->Rectangle 30 50 [1 1 1 1])
                                                     (layout/->Preferred (layout/->Margin (* requested-height (/ (:value state) (:maximum state)))
                                                                                          margin
                                                                                          0
                                                                                          margin
                                                                                          [(drawable/->Rectangle (- width (* 2 margin))
                                                                                                                 10 #_(* requested-height
                                                                                                                         (/ (:size state)
                                                                                                                            (:maximum state)))
                                                                                                                 [0.5 0.5 0.5 1])]))])))))

(defn create-scroll-bar [state-path event-channel control-channel]
  {:value 4
   :size 5
   :maximum 20})

(def scroll-bar
  {:view scroll-bar-view
   :constructor create-scroll-bar})

(quad-gui/def-view view [state]
  (layout/->FloatRight [(layout/->Flow (repeat 10
                                               (drawable/->Text "Blaaaaaaaaa "
                                                                (font/create "LiberationSans-Regular.ttf" 15)
                                                                [1 1 1 1])))
                        (quad-gui/call-view :scroll-bar
                                            scroll-bar
                                            {})]))

(defn create [state-path event-channel control-channel]
  {})

(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (quad-gui/start-view {:constructor create
                        :view view})
  (flow-gl.debug/write-timed-log))


(run-tests)

(ns examples.scrolling
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-gui :as quad-gui]
                         [events :as events]
                         [layoutable :as layoutable])

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
                                                                                          [(assoc (drawable/->Rectangle (- width (* 2 margin))
                                                                                                                        (* requested-height
                                                                                                                           (/ (:size state)
                                                                                                                              (:maximum state)))
                                                                                                                        [0.5 0.5 0.5 1])
                                                                                             :on-drag (fn [state x y]
                                                                                                        (let [change (* y
                                                                                                                        (/ (:maximum state)
                                                                                                                           requested-height))
                                                                                                              new-value (float (min (- (:maximum state)
                                                                                                                                       (:size state))
                                                                                                                                    (max 0
                                                                                                                                         (+ (:value state)
                                                                                                                                            change))))]
                                                                                                          (when (:on-drag state)
                                                                                                            ((:on-drag state) new-value))
                                                                                                          (assoc state
                                                                                                            :value new-value))))]))])))))

(defn create-scroll-bar [state-path event-channel control-channel]
  {:value 0
   :size 1
   :maximum 1})

(def scroll-bar
  {:view scroll-bar-view
   :constructor create-scroll-bar})

(quad-gui/def-view view [state]
  (let [content (layout/->Flow (repeat 10
                                       (drawable/->Text "Blaaaaaaaaa "
                                                        (font/create "LiberationSans-Regular.ttf" 15)
                                                        [(:value state) 1 1 1])))]
    (layout/->SizeDependent (fn [available-width available-height]
                              {:width available-width
                               :height (layoutable/preferred-size content
                                                                  (- available-width 30)
                                                                  available-height)})

                            (fn [requested-width requested-height]
                              (layout/->FloatRight [(layout/->Margin (:value state) 0 0 0
                                                                     content)
                                                    (quad-gui/call-view :scroll-bar
                                                                        scroll-bar
                                                                        {:maximum (:height (layoutable/preferred-size content
                                                                                                                      (- requested-width 30)
                                                                                                                      requested-height))
                                                                         :size requested-height
                                                                         :on-drag (quad-gui/apply-to-current-state [state new-value]
                                                                                                                   (assoc state :value new-value))})])))))

(defn create [state-path event-channel control-channel]
  {:value 0})

(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (quad-gui/start-view {:constructor create
                        :view view})
  (flow-gl.debug/write-timed-log))


(run-tests)

(ns examples.scrolling
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
                         [events :as events]
                         [layoutable :as layoutable])

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(defn scroll-bar-view-function [value maximum size on-drag]
  (let [width 30
        margin 5]
    (layouts/->SizeDependent (fn [available-width available-height]
                               {:width width :height 0})
                             (fn [state requested-width requested-height]
                               (layouts/->Superimpose [(drawable/->Rectangle 30 50 [1 1 1 1])
                                                       (layouts/->Preferred (layouts/->Margin (* requested-height (/ value maximum))
                                                                                              margin
                                                                                              0
                                                                                              margin
                                                                                              [(assoc (drawable/->Rectangle (- width (* 2 margin))
                                                                                                                            (* requested-height
                                                                                                                               (/ size
                                                                                                                                  maximum))
                                                                                                                            [0.5 0.5 0.5 1])
                                                                                                 :on-drag (fn [state x y]
                                                                                                            (let [change (* y
                                                                                                                            (/ maximum
                                                                                                                               requested-height))
                                                                                                                  new-value (float (min (- maximum
                                                                                                                                           size)
                                                                                                                                        (max 0
                                                                                                                                             (+ value
                                                                                                                                                change))))]
                                                                                                              (on-drag state new-value))))]))])))))

(quad-gui/def-view scroll-bar-view [state]
  (let [width 30
        margin 5]
    (layouts/->SizeDependent (fn [available-width available-height]
                               {:width width :height 0})
                             (fn [state requested-width requested-height]
                               (layouts/->Superimpose [(drawable/->Rectangle 30 50 [1 1 1 1])
                                                       (layouts/->Preferred (layouts/->Margin (* requested-height (/ (:value state) (:maximum state)))
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
  (let [content (layouts/->Flow (for [i (range 10000 10030)]
                                  (drawable/->Text (str i)
                                                   (font/create "LiberationSans-Regular.ttf" 15)
                                                   [1 1 1 1])))]
    (layouts/->SizeDependent (fn [available-width available-height]
                               {:width available-width
                                :height (:height (layoutable/preferred-size content
                                                                            (- available-width 30)
                                                                            available-height))})

                             (fn [state requested-width requested-height]
                               (layouts/->FloatRight [(layouts/->Margin (- (:value state)) 0 0 0
                                                                        [content] )
                                                      (scroll-bar-view-function (:value state)
                                                                                (:height (layoutable/preferred-size content
                                                                                                                    (- requested-width 30)
                                                                                                                    requested-height))
                                                                                requested-height
                                                                                (fn [state new-value]
                                                                                  (assoc state :value new-value)))])))))



(defn create [state-path event-channel control-channel]
  {:value 0})

(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (quad-gui/start-view {:constructor create
                        :view view})
  (flow-gl.debug/write-timed-log))


(run-tests)

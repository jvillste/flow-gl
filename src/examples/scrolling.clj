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



(layout/deflayout-not-memoized HorizontalSplit [left-width left middle right]
  (layout [this requested-width requested-height]
          (assoc this
            :children
            (let [middle-size (layoutable/preferred-size middle requested-width requested-height)
                  left-width (max 0 (min (- requested-width
                                            (:width middle-size))
                                         left-width))]
              [(layout/set-dimensions-and-layout left 0 0 left-width requested-height)
               (layout/set-dimensions-and-layout middle left-width 0 (:width middle-size) requested-height)
               (layout/set-dimensions-and-layout right (+ left-width (:width middle-size)) 0 (max 0 (- requested-width left-width (:width middle-size))) requested-height)])))

  (preferred-size [this available-width available-height]
                  (let [middle-size (layoutable/preferred-size middle available-width available-height)
                        left-size (layoutable/preferred-size left (max 0 (min (- available-width
                                                                                 (:width middle-size))
                                                                              left-width))
                                                             available-height)
                        right-size (layoutable/preferred-size right (max 0 (- available-width left-width (:width middle-size))) available-height)]
                    {:width (+ (:width left-size)
                               (:width middle-size)
                               (:width right-size))

                     :height (max (:height left-size)
                                  (:height middle-size)
                                  (:height right-size))})))

(quad-gui/def-view horizontal-split-view [state]
  (->HorizontalSplit (:left-width state)
                     (:left state)

                     (assoc (drawable/->Rectangle 15 0 [0 0 1 1])
                       :on-drag (fn [state x y]
                                  (update-in state [:left-width] + x)))

                     (:right state)))

(defn create-horizontal-split-view [state-path event-channel control-channel]
  {:left-width 100})

(def horizontal-split
  {:view horizontal-split-view
   :constructor create-horizontal-split-view})

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

(quad-gui/def-view scroll-panel-view [state]
  (layouts/->SizeDependent (fn [available-width available-height]
                             {:width available-width
                              :height (:height (layoutable/preferred-size (:content state)
                                                                          (- available-width 30)
                                                                          available-height))})

                           (fn [state requested-width requested-height]
                             (let [maximum (:height (layoutable/preferred-size (:content state)
                                                                               (- requested-width 30)
                                                                               requested-height))
                                   scroll-position (max 0
                                                        (min (:scroll-position state)
                                                             (- maximum requested-height)))]
                               (swap! quad-gui/current-view-state-atom assoc :scroll-position scroll-position)
                               (layouts/->FloatRight [(layouts/->Margin (- (:scroll-position state)) 0 0 0
                                                                        [(:content state)])
                                                      (scroll-bar-view-function scroll-position
                                                                                maximum
                                                                                requested-height
                                                                                (fn [state new-value]
                                                                                  (assoc state :scroll-position new-value)))]))
                             )))

(defn create-scroll-panel [state-path event-channel control-channel]
  {:scroll-position 0})

(def scroll-panel
  {:view scroll-panel-view
   :constructor create-scroll-panel})


(quad-gui/def-view view [state]
  (quad-gui/call-view horizontal-split
                      {:left (quad-gui/call-view scroll-panel
                                                 {:content (layouts/->Flow (for [i (range 10000 10030)]
                                                                             (drawable/->Text (str i)
                                                                                              (font/create "LiberationSans-Regular.ttf" 15)
                                                                                              [1 1 1 1])))})
                       :right (quad-gui/call-view scroll-panel
                                                  {:content (layouts/->Flow (for [i (range 20000 20030)]
                                                                              (drawable/->Text (str i)
                                                                                               (font/create "LiberationSans-Regular.ttf" 15)
                                                                                               [1 1 1 1])))})}))

(defn create [state-path event-channel control-channel]
  {})

(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (try (quad-gui/start-view {:constructor create
                             :view view})
       (finally (flow-gl.debug/write-timed-log))))


#_(run-tests)

;; flow-gl.debug/debug-timed

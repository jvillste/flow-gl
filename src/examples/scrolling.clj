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
                  left-width (max 0
                                  (min (- requested-width
                                          (:width middle-size))
                                       left-width))]
              [(layout/set-dimensions-and-layout left
                                                 0
                                                 0
                                                 left-width
                                                 requested-height)
               (layout/set-dimensions-and-layout middle
                                                 left-width
                                                 0
                                                 (:width middle-size)
                                                 requested-height)
               (layout/set-dimensions-and-layout right
                                                 (+ left-width
                                                    (:width middle-size))
                                                 0
                                                 (max 0
                                                      (- requested-width
                                                         left-width
                                                         (:width middle-size)))
                                                 requested-height)])))

  (preferred-size [this available-width available-height]
                  (let [middle-size (layoutable/preferred-size middle
                                                               available-width
                                                               available-height)
                        left-size (layoutable/preferred-size left
                                                             (max 0
                                                                  (min (- available-width
                                                                          (:width middle-size))
                                                                       left-width))
                                                             available-height)
                        right-size (layoutable/preferred-size right
                                                              (max 0
                                                                   (- available-width
                                                                      left-width
                                                                      (:width middle-size)))
                                                              available-height)]
                    {:width (+ (:width left-size)
                               (:width middle-size)
                               (:width right-size))

                     :height (max (:height left-size)
                                  (:height middle-size)
                                  (:height right-size))})))

(quad-gui/def-control horizontal-split
  ([state-path event-channel control-channel]
     {:left-width 100})

  ([state left right]
     (->HorizontalSplit (:left-width state)
                        left

                        (assoc (drawable/->Rectangle 15 0 [0 0 1 1])
                          :on-drag (fn [state x y]
                                     (update-in state [:left-width] + x)))

                        right)))

(defn scroll-bar [value maximum size width height on-drag]
  (let [margin 5]
    (layouts/->Superimpose [(drawable/->Rectangle 30 50 [1 1 1 1])
                            (layouts/->Preferred (layouts/->Margin (* height (/ value maximum))
                                                                   margin
                                                                   0
                                                                   margin
                                                                   [(assoc (drawable/->Rectangle (- width (* 2 margin))
                                                                                                 (* height
                                                                                                    (/ size
                                                                                                       maximum))
                                                                                                 [0.5 0.5 0.5 1])
                                                                      :on-drag (fn [state x y]
                                                                                 (let [change (* y
                                                                                                 (/ maximum
                                                                                                    height))
                                                                                       new-value (float (min (- maximum
                                                                                                                size)
                                                                                                             (max 0
                                                                                                                  (+ value
                                                                                                                     change))))]
                                                                                   (on-drag state new-value))))]))])))

(quad-gui/def-control scroll-panel
  ([state-path event-channel control-channel]
     {:scroll-position 0})

  ([state content]
     (let [scroll-bar-width 30]
       (layouts/->SizeDependent (fn [available-width available-height]
                                  {:width available-width
                                   :height (:height (layoutable/preferred-size content
                                                                               (- available-width scroll-bar-width)
                                                                               available-height))})

                                (fn [state requested-width requested-height]
                                  (let [maximum (:height (layoutable/preferred-size content
                                                                                    (- requested-width 30)
                                                                                    requested-height))
                                        scroll-position (max 0
                                                             (min (:scroll-position state)
                                                                  (- maximum requested-height)))]

                                    (swap! quad-gui/current-view-state-atom assoc :scroll-position scroll-position)

                                    (layouts/->FloatRight [(layouts/->Margin (- (:scroll-position state)) 0 0 0
                                                                             [content])
                                                           (scroll-bar scroll-position
                                                                       maximum
                                                                       requested-height
                                                                       scroll-bar-width
                                                                       requested-height
                                                                       (fn [state new-value]
                                                                         (assoc state :scroll-position new-value)))])))))))

(defn text [value]
  (layouts/->Margin 2 2 0 0
                    [(drawable/->Text (str value)
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      [1 1 1 1])]))

(quad-gui/def-control root
  ([state-path event-channel control-channel]
     {})

  ([state]
     (horizontal-split (scroll-panel (layouts/->Flow (for [i (range 10000 10030)]
                                                       (text i))))

                       (scroll-panel (layouts/->Flow (for [i (range 20000 20030)]
                                                       (text i)))))))

(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (try (quad-gui/start-view create-root root-view)
       (finally (flow-gl.debug/write-timed-log))))


#_(run-tests)

;; flow-gl.debug/debug-timed

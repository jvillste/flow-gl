(ns flow-gl.gui.controls
  (:require [clojure.core.async :as async]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable])
            [flow-gl.csp :as csp]

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(defn create-text-editor-keyboard-event-handler [view-context]
  (fn [state event]
    (cond
     (events/key-pressed? event :back-space)
     (gui/update-binding state
                         view-context
                         (fn [text] (apply str (drop-last text)))
                         :text)

     (and (:character event)
          (= (:type event)
             :key-pressed))
     (gui/update-binding state
                         view-context
                         #(str % (:character event))
                         :text)

     :default
     state)))

(defn text-editor-view [view-context state]
  (if (:view state)
    ((:view state) view-context state)
    (layouts/->Box 10 [(drawable/->Rectangle 0
                                             0
                                             (cond
                                              (:has-focus state) [0 200 200 255]
                                              (:mouse-over state) [0 255 255 255]
                                              :default [0 100 100 255]))
                       (drawable/->Text (or (:text state) "")
                                        (font/create "LiberationSans-Regular.ttf" 15)
                                        (if (:has-focus state)
                                          [255 255 255 255]
                                          [100 100 100 255]))])))

(defn text-editor [view-context]
  {:local-state {:text "haa"}
   :handle-keyboard-event (create-text-editor-keyboard-event-handler view-context)
   :can-gain-focus true
   :view text-editor-view})

(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))



(defn scroll-panel-view [view-context state]
  (gui/->SizeDependent (fn [child requested-width requested-height]
                             (let [{preferred-width :width preferred-height :height} (layoutable/preferred-size child requested-width requested-height)
                                   maximum-x-scroll (- preferred-width requested-width)
                                   maximum-y-scroll (- preferred-height requested-height)
                                   scroll-bar-width 5
                                   scroll-bar-color [255 255 255 120]]
                               (-> (l/superimpose (layouts/->Margin (- (:scroll-position-y state)) 0 0 (- (:scroll-position-x state))
                                                                    [(l/preferred child)])
                                                  (when true #_(:mouse-over state)
                                                        (l/absolute (when (< requested-height preferred-height)
                                                                      (let [scroll-bar-length (* requested-height
                                                                                                 (/ requested-height preferred-height))]
                                                                        (assoc (drawable/->Rectangle scroll-bar-width
                                                                                                     scroll-bar-length
                                                                                                     scroll-bar-color)
                                                                          :x (- requested-width scroll-bar-width)

                                                                          :y (* (/ (:scroll-position-y state)
                                                                                   maximum-y-scroll)
                                                                                (- requested-height
                                                                                   scroll-bar-length)))))
                                                                    (when (< requested-width preferred-width)
                                                                      (let [scroll-bar-length (* requested-width
                                                                                                 (/ requested-width preferred-width))]
                                                                        (assoc (drawable/->Rectangle scroll-bar-length
                                                                                                     scroll-bar-width
                                                                                                     scroll-bar-color)
                                                                          :x (* (/ (:scroll-position-x state)
                                                                                   maximum-x-scroll)
                                                                                (- requested-width
                                                                                   scroll-bar-length))
                                                                          :y (- requested-height scroll-bar-width)))))))
                                   (gui/add-mouse-event-handler-with-context view-context
                                                                             (fn [state event]
                                                                               (cond (= (:type event)
                                                                                        :mouse-wheel-moved)
                                                                                     (-> state
                                                                                         (update-in [:scroll-position-x] (fn [position]
                                                                                                                           (max 0 (min maximum-x-scroll
                                                                                                                                       (- position
                                                                                                                                          (:x-distance event))))))
                                                                                         (update-in [:scroll-position-y] (fn [position]
                                                                                                                           (max 0 (min maximum-y-scroll
                                                                                                                                       (- position
                                                                                                                                          (:y-distance event)))))))

                                                                                     (= (:type event)
                                                                                        :mouse-enter)
                                                                                     (assoc state :mouse-over true)

                                                                                     (= (:type event)
                                                                                        :mouse-leave)
                                                                                     (assoc state :mouse-over false)

                                                                                     :default state))))))
                           [(:content state)]))

(defn create-scroll-panel [view-context]
  {:local-state {:scroll-position-x 0
                 :scroll-position-y 0}
   :view #'scroll-panel-view})

(defn scroll-panel [id content]
  content
  #_(gui/call-view create-scroll-panel
                 id
                 {:content content}))




(defn handle-button-click [state]
  (if (not (:disabled state))
    (do (if-let [on-pressed (:on-pressed state)]
          (on-pressed))
        state)
    state))

#_(gui/def-control button
    ([view-context  control-channel]
       {:text text
        :has-focus false
        :on-pressed nil
        :can-gain-focus true
        :handle-keyboard-event (fn [state event]
                                 (events/on-key state event
                                                :enter (handle-button-click state)))})

    ([view-context state]
       (-> (layouts/->Box 10 [(drawable/->Rectangle 0
                                                    0
                                                    (if (:has-focus state)
                                                      [0 0.8 0.8 1]
                                                      [0 0.5 0.5 1]))
                              (drawable/->Text (:text state)
                                               (font/create "LiberationSans-Regular.ttf" 15)
                                               (if (:disabled state)
                                                 [0.5 0.5 0.5 1]
                                                 [0 0 0 1]))])
           (gui/on-mouse-event :mouse-clicked
                               view-context
                               (fn [state event]
                                 (handle-button-click state))))))

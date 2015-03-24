(ns flow-gl.gui.controls
  (:require [clojure.core.async :as async]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable])
            [flow-gl.csp :as csp]

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(defn create-text-editor-keyboard-event-handler [view-context]
  (fn [state]
    (let [event (:event state)]
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
       state))))

(defn text-editor-view [view-context state]
  (if (:view state)
    ((:view state) view-context state)
    (layouts/->Box 10 [(drawable/->Rectangle 0
                                             0
                                             (cond
                                              (:has-focus state) [0 200 200 255]
                                              (:mouse-over state) [0 255 255 255]
                                              :default [0 100 100 255]))
                       (drawable/->Text (:text state)
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

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



(defn handle-new-text [state new-text]
  (when (:on-change state)
    #_(async/go (async/>! (:on-change state) new-text))
    ((:on-change state) new-text))
  (assoc-in state [:text] new-text))

(defn handle-text-editor-event [state event]
  (cond
   (events/key-pressed? event :back-space)
   [(handle-new-text state (apply str (drop-last (:text state))))
    false]

   (and (:character event)
        (= (:type event)
           :key-pressed))
   [(handle-new-text state (str (:text state)
                                (:character event)))
    false]

   :default
   [state true]))


(gui/def-control text-editor
  ([view-context control-channel]
     {:text ""
      :handle-keyboard-event handle-text-editor-event
      :can-gain-focus true})

  ([view-context state]
     (if (:view state)
       ((:view state) view-context state)
       (layouts/->Box 10 [(drawable/->Rectangle 0
                                                0
                                                (cond
                                                 (:has-focus state) [0 0.8 0.8 1]
                                                 (:mouse-over state) [0 0.7 0.7 1]
                                                 :default [0 0.5 0.5 1]))
                          (drawable/->Text (:text state)
                                           (font/create "LiberationSans-Regular.ttf" 15)
                                           (if (:has-focus state)
                                             [0 0 0 255]
                                             [0.3 0.3 0.3 1]))]))))

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

(gui/def-control button
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

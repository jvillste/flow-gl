(ns examples.registration-form-2
  (:require [clojure.core.async :as async]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
                         [events :as events]
                         [layoutable :as layoutable])

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


(quad-gui/def-control text-editor
  ([state-path event-channel control-channel]
     {:text ""
      :edited-text ""
      :editing? false
      :has-focus false
      :handle-keyboard-event handle-text-editor-event})

  ([state]
     (layouts/->Box 10 [(drawable/->Rectangle 0
                                              0
                                              (if (:has-focus state)
                                                [0 0.8 0.8 1]
                                                [0 0.5 0.5 1]))
                        (drawable/->Text (if (:editing? state)
                                           (:edited-text state)
                                           (:text state))
                                         (font/create "LiberationSans-Regular.ttf" 15)
                                         (if (:has-focus state)
                                           (if (:editing? state)
                                             [0 0 1 1]
                                             [0 0 0 1])
                                           [0.3 0.3 0.3 1]))])))


(quad-gui/def-control form
  ([state-path event-channel control-channel]
     (conj {:user-name "foo"
            :full-name ""}
           quad-gui/child-focus-handlers))

  ([state]

     (layouts/->VerticalStack [(layouts/->HorizontalStack [(text-editor :text (:user-name state)
                                                                        :on-change (quad-gui/apply-to-current-state [state new-text]
                                                                                                                    (assoc state :user-name new-text)))
                                                           (if (:user-name-in-use state)
                                                             (text "Not available")
                                                             (drawable/->Empty 0 0))
                                                           (if (:checking-availability state)
                                                             (text "Checking availability")
                                                             (drawable/->Empty 0 0))])
                               (text-editor :text (:full-name state)
                                            :on-change (quad-gui/apply-to-current-state [state new-text]
                                                                                        (assoc state :full-name new-text)))

                               #_(button-view (:submit-button state))])))



(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (try (quad-gui/start-view create-form form-view)
       (finally (flow-gl.debug/write-timed-log))))


#_(run-tests)

;; flow-gl.debug/debug-timed

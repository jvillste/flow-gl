(ns examples.registration-form-2
  (:require [clojure.core.async :as async]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
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


(quad-gui/def-control text-editor
  ([state-path event-channel control-channel]
     {:text ""
      :handle-keyboard-event handle-text-editor-event
      :can-gain-focus true})

  ([state]
     (layouts/->Box 10 [(drawable/->Rectangle 0
                                              0
                                              (cond
                                               (:has-focus state) [0 0.8 0.8 1]
                                               (:mouse-over state) [0 0.7 0.7 1]
                                               :default [0 0.5 0.5 1]))
                        (drawable/->Text (:text state)
                                         (font/create "LiberationSans-Regular.ttf" 15)
                                         (if (:has-focus state)
                                           [0 0 0 1]
                                           [0.3 0.3 0.3 1]))])))


(defn user-name-available? [user-name]
  (let [channel (async/chan)]
    (async/thread (async/put! channel (do (Thread/sleep (+ 1000
                                                           (rand 2000)))
                                          (println "sending result for" user-name)
                                          (not (= "foo" user-name)))))

    channel))

(defn text
  ([value]
     (text value [1 1 1 1]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))

(quad-gui/def-control button
  ([state-path event-channel control-channel]
     {:text text
      :has-focus false
      :on-pressed nil
      :handle-keyboard-event (fn [state event]
                               (events/on-key state event
                                              :enter (if (not (:disabled state))
                                                       (do (if-let [on-pressed (:on-pressed state)]
                                                             (on-pressed))
                                                           state)
                                                       state)))})

  ([state]
     (layouts/->Box 10 [(drawable/->FilledRoundedRectangle 0
                                                           0
                                                           10
                                                           (if (:has-focus state)
                                                             [0 0.8 0.8 1]
                                                             [0 0.5 0.5 1]))
                        (drawable/->Text (:text state)
                                         (font/create "LiberationSans-Regular.ttf" 15)
                                         (if (:disabled state)
                                           [0.5 0.5 0.5 1]
                                           [0 0 0 1]))])))


(quad-gui/def-control form
  ([state-path event-channel control-channel]
     (let [state (conj {:user-name ""
                        :querying? false
                        :user-name-available? :unknown
                        :user-name-channel (async/chan)
                        :full-name ""}
                       quad-gui/child-focus-handlers)]

       (async/go (let [[throttled-query unthrottled-query] (csp/throttle (:user-name-channel state) 500)]
                   (loop [query-control-channel nil]
                     (async/alt! control-channel ([_] (println "exiting user-name process"))
                                 throttled-query ([user-name]
                                                    (if (= user-name "")
                                                      (recur nil)
                                                      (let [new-query-control-channel (async/chan)]
                                                        (quad-gui/transact state-path event-channel
                                                                           (fn [state]
                                                                             (assoc state :querying? true)))
                                                        (async/go (let [result-channel (user-name-available? user-name)]
                                                                    (async/alt! new-query-control-channel ([_])
                                                                                result-channel ([result]
                                                                                                  (quad-gui/transact state-path event-channel
                                                                                                                     (fn [state]
                                                                                                                       (assoc state
                                                                                                                         :user-name-available? (if result :available :unavailable)
                                                                                                                         :querying? false)))))))
                                                        (recur new-query-control-channel))))


                                 unthrottled-query ([user-name]
                                                      (if query-control-channel
                                                        (async/close! query-control-channel))

                                                      (quad-gui/transact state-path event-channel
                                                                         (fn [state]

                                                                           (assoc state
                                                                             :querying? false
                                                                             :user-name user-name
                                                                             :user-name-available? :unknown)))
                                                      (recur nil))))))
       state))

  ([state]

     (layouts/->VerticalStack [(layouts/->HorizontalStack [(text-editor :text (:user-name state)
                                                                        :on-change (fn [new-text]
                                                                                     (async/go (async/>! (:user-name-channel state) new-text))))
                                                           (if (and (not= (:user-name-available? state) :unknown)
                                                                    (not (:querying? state)))
                                                             (if (= (:user-name-available? state)
                                                                    :available)
                                                               (text "Available" [0 1 0 1])
                                                               (text "Not available" [1 0 0 1]))
                                                             (drawable/->Empty 0 0))
                                                           (if (:querying? state)
                                                             (text "Checking availability")
                                                             (drawable/->Empty 0 0))])
                               (text-editor :text (:full-name state)
                                            :on-change (quad-gui/apply-to-current-state [state new-text]
                                                                                        (assoc state :full-name new-text)))

                               (button :text "Send"
                                       :on-pressed (fn [] (println "send pressed"))
                                       :disabled (or (= "" (:full-name state))
                                                     (= "" (:user-name state))
                                                     (:querying? state)
                                                     (= :unavailable (:user-name-available? state))
                                                     (= :unknown (:user-name-available? state))))])))



(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (try (quad-gui/start-view create-form form-view)
       (finally (flow-gl.debug/write-timed-log))))


#_(run-tests)

;; flow-gl.debug/debug-timed

(ns examples.registration-form-2
  (:require [clojure.core.async :as async]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls])
            [flow-gl.csp :as csp]

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))


(defn user-name-available? [user-name]
  (let [channel (async/chan)]
    (async/thread (async/put! channel (do (Thread/sleep (+ 1000
                                                           (rand 2000)))
                                          (println "sending result for" user-name)
                                          (not (= "foo" user-name)))))

    channel))

(defn register [user-name]
  (let [channel (async/chan)]
    (async/thread (async/put! channel (do (Thread/sleep (+ 1000
                                                           (rand 2000)))
                                          (println "sending " user-name)
                                          (str "Username " user-name " registered!"))))

    channel))


(quad-gui/def-control form
  ([view-context control-channel]
     (let [state (conj {:user-name ""
                        :querying? false
                        :registering? false
                        :user-name-available? :unknown
                        :user-name-channel (async/chan)
                        :register-channel (async/chan)
                        :full-name ""}
                       quad-gui/child-focus-handlers)]

       (async/go-loop []
                      (async/alt! control-channel ([_] (println "exiting register process"))
                                  (:register-channel state) ([user-name]
                                                               (println "got " user-name)
                                                               (quad-gui/apply-to-state view-context assoc
                                                                                        :registering? true)
                                                               (async/go (let [register-message (async/<! (register user-name))]
                                                                           (println "got message")
                                                                           (quad-gui/apply-to-state view-context assoc
                                                                                                    :register-message register-message)
                                                                           (async/<! (async/timeout 1000))
                                                                           (quad-gui/apply-to-state view-context assoc
                                                                                                    :registering? false
                                                                                                    :register-message "")))
                                                               (recur))))

       (async/go (let [[throttled-user-name unthrottled-user-name] (csp/throttle (:user-name-channel state) 500)]
                   (loop [user-name-control-channel nil]
                     (async/alt! control-channel ([_] (println "exiting user-name process"))
                                 throttled-user-name ([user-name]
                                                        (if (= user-name "")
                                                          (recur nil)
                                                          (let [new-query-control-channel (async/chan)]
                                                            (quad-gui/apply-to-state view-context assoc
                                                                                     :querying? true)
                                                            (async/go (let [result-channel (user-name-available? user-name)]
                                                                        (async/alt! new-query-control-channel ([_])
                                                                                    result-channel ([result]
                                                                                                      (quad-gui/apply-to-state view-context assoc
                                                                                                                               :user-name-available? (if result :available :unavailable)
                                                                                                                               :querying? false)))))
                                                            (recur new-query-control-channel))))


                                 unthrottled-user-name ([user-name]
                                                          (if user-name-control-channel
                                                            (async/close! user-name-control-channel))

                                                          (quad-gui/apply-to-state view-context assoc
                                                                                   :querying? false
                                                                                   :user-name user-name
                                                                                   :user-name-available? :unknown)
                                                          (recur nil))))))
       state))

  ([view-context state]
     (layouts/->VerticalStack [(layouts/->HorizontalStack [(controls/text-editor :text (:user-name state)
                                                                                 :on-change (fn [new-text]
                                                                                              (async/go (async/>! (:user-name-channel state) new-text))))
                                                           (if (and (not= (:user-name-available? state) :unknown)
                                                                    (not (:querying? state)))
                                                             (if (= (:user-name-available? state)
                                                                    :available)
                                                               (controls/text "Available" [0 1 0 1])
                                                               (controls/text "Not available" [1 0 0 1]))
                                                             (drawable/->Empty 0 0))
                                                           (if (:querying? state)
                                                             (controls/text "Checking availability")
                                                             (drawable/->Empty 0 0))])
                               (controls/text-editor :text (:full-name state)
                                                     :on-change #(quad-gui/apply-to-state view-context assoc :full-name %))

                               (controls/button :text "Send"
                                                :on-pressed (fn [] (async/go (async/>! (:register-channel state) (:user-name state))))
                                                :disabled (or (= "" (:full-name state))
                                                              (= "" (:user-name state))
                                                              (:querying? state)
                                                              (= :unavailable (:user-name-available? state))
                                                              (= :unknown (:user-name-available? state))
                                                              (:registering? state)))
                               (if (:registering? state)
                                 (controls/text "Registering")
                                 (drawable/->Empty 0 0))
                               (controls/text (:register-message state))])))



(flow-gl.debug/set-active-channels :all)

(defn start []
  (flow-gl.debug/reset-log)
  (try (quad-gui/start-view create-form form-view)
       #_(.start (Thread. (fn [] (quad-gui/start-view create-form form-view))))
       (finally (flow-gl.debug/write-timed-log))))


#_(run-tests)

;; flow-gl.debug/debug-timed

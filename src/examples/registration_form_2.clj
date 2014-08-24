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


(defn result-as-channel [function]
  (let [channel (async/chan)]
    (async/thread (async/put! channel (function)))
    channel))

(defn user-name-available? [user-name]
  (result-as-channel #(do (Thread/sleep (+ 1000
                                           (rand 2000)))
                          (not (= "foo" user-name)))))

(defn register [user-name]
  (result-as-channel #(do (Thread/sleep (+ 1000
                                           (rand 2000)))
                          (str "Username " user-name " registered!"))))


(quad-gui/def-control form
  ([view-context control-channel]

     (let [user-name-channel (async/chan)
           register-channel (async/chan)]

       (async/go-loop []
                      (async/alt! control-channel ([_] (println "exiting register process"))
                                  register-channel ([user-name]
                                                      (quad-gui/apply-to-state view-context assoc
                                                                               :registering? true)
                                                      (async/go (let [register-message (async/<! (register user-name))]
                                                                  (quad-gui/apply-to-state view-context assoc
                                                                                           :register-message register-message)
                                                                  (async/<! (async/timeout 1000))
                                                                  (quad-gui/apply-to-state view-context assoc
                                                                                           :registering? false
                                                                                           :register-message "")))
                                                      (recur))))

       (let [[throttled-user-name unthrottled-user-name] (csp/throttle user-name-channel 500)]
         (async/go-loop [user-name-control-channel nil]
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
                                                             (recur nil)))))

       (conj {:user-name ""
              :querying? false
              :registering? false
              :user-name-available? :unknown
              :user-name-channel user-name-channel
              :register-channel register-channel
              :full-name ""}
             quad-gui/child-focus-handlers)))

  ([view-context state]
     (layouts/->VerticalStack [(layouts/->HorizontalStack [(controls/text-editor :text (:user-name state)
                                                                                 :on-change (fn [new-user-name]
                                                                                              (async/put! (:user-name-channel state)
                                                                                                          new-user-name)))
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
                                                :on-pressed (fn [] (async/put! (:register-channel state)
                                                                               (:user-name state)))
                                                :disabled (or (= (:full-name state)
                                                                 "")
                                                              (= (:user-name state)
                                                                 "")
                                                              (:querying? state)
                                                              (= (:user-name-available? state)
                                                                 :unavailable)
                                                              (= (:user-name-available? state)
                                                                 :unknown)
                                                              (:registering? state)))
                               (if (:registering? state)
                                 (controls/text "Registering")
                                 (drawable/->Empty 0 0))
                               (controls/text (:register-message state))])))


(defn start []
  (quad-gui/start-view create-form form-view))

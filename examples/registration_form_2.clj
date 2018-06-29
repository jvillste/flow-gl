(ns examples.registration-form-2
  (:require [clojure.core.async :as async]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [gui :as gui]
                         [layout-dsl :as l])
            (flow-gl.gui.components [async-text-editor :as async-text-editor])
            [flow-gl.csp :as csp]

            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(defn user-name-available? [user-name]
  (async/thread (Thread/sleep (+ 1000
                                 (rand 2000)))
                (not (= "foo" user-name))))

(defn register [user-name]
  (async/thread (Thread/sleep (+ 1000
                                 (rand 2000)))
                (str "Username " user-name " registered!")))

(defn form-view [view-context state]
  (l/vertically (l/horizontally (l/minimum-size 300 0 (gui/call-view async-text-editor/text-editor
                                                                     :user-name-editor
                                                                     {:text (:user-name state)
                                                                      :on-change (:user-name-channel state)}))

                                (let [[text color] (case (:user-name-available? state)
                                                     :available ["Available" [0 255 0 255]]
                                                     :unavailable ["Unavailable" [255 0 0 255]]
                                                     :unknown [nil nil])]
                                  (when text
                                    (controls/text text color)))

                                (when (:querying? state)
                                  (controls/text "Checking availability")))

                (l/margin 10 0 0 0
                          (l/minimum-size 300 0
                                          (gui/call-and-bind view-context
                                                             state
                                                             :full-name
                                                             :text
                                                             controls/text-editor
                                                             :full-name-editor)))

                (l/margin 10 0 0 0
                          (l/minimum-size 300 0
                                          (controls/button view-context
                                                           "Send"
                                                           (or (= (:full-name state)
                                                                  "")
                                                               (= (:user-name state)
                                                                  "")
                                                               (:querying? state)
                                                               (= (:user-name-available? state)
                                                                  :unavailable)
                                                               (= (:user-name-available? state)
                                                                  :unknown)
                                                               (:registering? state))
                                                           (fn [state]
                                                             (async/put! (:register-channel state)
                                                                         (:user-name state))
                                                             state))))
                
                (when (:registering? state)
                  (controls/text "Registering"))
                
                (controls/text (:register-message state))))

(defn form [view-context]
  (let [user-name-channel (async/chan)
        register-channel (async/chan)]

    (async/go-loop []
      (async/alt! (:control-channel view-context) ([_] (println "exiting register process"))
                  register-channel ([user-name]
                                    (gui/apply-to-state view-context assoc
                                                        :registering? true)
                                    (async/go (let [register-message (async/<! (register user-name))]
                                                (gui/apply-to-state view-context assoc
                                                                    :register-message register-message)
                                                (async/<! (async/timeout 1000))
                                                (gui/apply-to-state view-context assoc
                                                                    :registering? false
                                                                    :register-message "")))
                                    (recur))))

    (let [[throttled-user-name unthrottled-user-name] (csp/throttle user-name-channel 500)]
      (async/go-loop [user-name-control-channel nil]
        (async/alt! (:control-channel view-context) ([_] (println "exiting user-name process"))
                    throttled-user-name ([user-name]
                                         (if (= user-name "")
                                           (recur nil)
                                           (let [new-query-control-channel (async/chan)]
                                             (gui/apply-to-state view-context assoc
                                                                 :querying? true)
                                             (async/go (let [result-channel (user-name-available? user-name)]
                                                         (async/alt! new-query-control-channel ([_])
                                                                     result-channel ([result]
                                                                                     (gui/apply-to-state view-context assoc
                                                                                                         :user-name-available? (if result :available :unavailable)
                                                                                                         :querying? false)))))
                                             (recur new-query-control-channel))))


                    unthrottled-user-name ([user-name]
                                           (if user-name-control-channel
                                             (async/close! user-name-control-channel))

                                           (gui/apply-to-state view-context assoc
                                                               :querying? false
                                                               :user-name user-name
                                                               :user-name-available? :unknown)
                                           (recur nil)))))

    (conj {:local-state {:user-name ""
                         :querying? false
                         :registering? false
                         :user-name-available? :unknown
                         :user-name-channel user-name-channel
                         :register-channel register-channel
                         :full-name ""}
           :view #'form-view} 
          gui/child-focus-handlers)))


(defn start []
  (.start (Thread. (fn []
                     (gui/start-control form)))))

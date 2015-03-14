(ns examples.window
  (:require [clojure.core.async :as async]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad :as quad])
            [datomic.api :as d]
            (flow-gl.gui [drawable :as drawable]
                         [gui :as gui]
                         [transformer :as transformer]
                         [renderer :as renderer]
                         [window :as window]
                         [layout :as layout]
                         [layouts :as layouts]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            [flow-gl.debug :as debug])
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable]
           [java.nio ByteBuffer])
  (:use flow-gl.utils
        midje.sweet
        clojure.test))



;; Control test

(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 25)
                      color)))

(def initial-counter-state {:count 0
                            :can-gain-focus true
                            :handle-keyboard-event (fn [state event]
                                                     [(update-in state [:count] inc)
                                                      true])})

(defn static [view-context]
  {:view (fn [view-context state]
           (text "foo"))})

(defn counter [view-context]

  (assoc initial-counter-state
    :view (fn [view-context state]
            (let [duration (mod (:frame-started view-context)
                                (:pulse-rate state))]

              (-> (text (str (:count state) (if (> (/ duration
                                                      (:pulse-rate state))
                                                   0.5)
                                              "x"
                                              "")
                             (if (:mouse-over state)
                               "o"
                               ""))
                        (if (:has-focus state)
                          [255 255 255 255]
                          [100 100 100 255]))
                  (assoc :sleep-time (- (/ (:pulse-rate state)
                                           2)
                                        duration)))))))


(defn app [view-context]

  (async/go-loop []
    (async/alt! (:control-channel view-context) ([_] (println "exiting counter process"))
                (async/timeout 2000) ([_]
                                        (gui/apply-to-state view-context update-in [:count] inc)
                                        (recur))))

  (merge initial-counter-state
         gui/child-focus-handlers
         {:view (fn [view-context state]
                  (l/vertically (drawable/->Rectangle 10 10 [0 255 0 255])
                                (gui/on-mouse-clicked (text (str "count " (:count state)))
                                                      view-context
                                                      (fn [state event]
                                                        (update-in state [:count] inc)))
                                #_(drawable/->Rectangle 10 10 [0 255 0 255])
                                (gui/call-view view-context counter :child-1 {:pulse-rate 2000})
                                #_(gui/call-view view-context counter :child-2 {:pulse-rate 500})
                                #_(transformer/with-transformers
                                    (transformer/->Filter :fade1
                                                          quad/alpha-fragment-shader-source
                                                          [:1f "alpha" 1])
                                    (text (-> view-context :application-state :focused-state-paths vec)))))}))


(defn static-app [view-context]

  (merge initial-counter-state
         gui/child-focus-handlers
         {:view (fn [view-context state]
                  (l/vertically (-> (text (str "count " (:count state) (if (:mouse-over state) "x" "")))

                                    (gui/on-mouse-clicked
                                     view-context
                                     (fn [state event]
                                       (update-in state [:count] inc)))

                                    (gui/add-mouse-event-handler-with-context
                                     view-context
                                     (fn [state event]
                                       state)))
                                (gui/call-view view-context static :static)
                                (gui/call-view view-context counter :child-1 {:pulse-rate 1000})
                                (text (-> view-context :application-state :mouse-over-layout-paths vec))
                                (text (-> view-context :application-state :mouse-over-paths vec))))}))




;; App test

(defn counter-view [state]
  (text (:count state) (if (:has-focus state)
                         [255 255 255 255]
                         [100 100 100 255])))

(defn layout-app [state event]
  (let [state (if (:view-state state)
                state
                (assoc state :view-state
                       (merge initial-counter-state

                              {:child-view-states {:child-1 initial-counter-state
                                                   :child-2 initial-counter-state}})))
        state (if (:focused-state-paths state)
                state
                (assoc state :focused-state-paths [[:view-state] [:child-view-states :child-1]]))]
    (-> state
        (assoc :layoutable (-> (l/vertically (text (get-in state [:view-state :count]))
                                             (-> (counter-view (get-in state [:view-state :child-view-states :child-1]))
                                                 (assoc :state-path [:view-state :child-view-states :child-1]))
                                             (-> (counter-view (get-in state [:view-state :child-view-states :child-2]))
                                                 (assoc :state-path [:view-state :child-view-states :child-2]))
                                             (transformer/with-transformers
                                               (transformer/->Filter :fade1
                                                                     quad/alpha-fragment-shader-source
                                                                     [:1f "alpha" 0.3])
                                               (text (:focused-state-paths state))))

                               (assoc :state-path [:view-state]))))))


(defn start []
  #_(gui/start-app layout-app)
  (gui/start-control app)
  #_(gui/start-control static-app))

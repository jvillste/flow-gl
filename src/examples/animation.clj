(ns examples.animation
  (:require [clojure.core.async :as async]
            [com.climate.claypoole :as claypoole]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [quad-gui :as quad-gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.debug :as debug]
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable]
           [java.util Random])
  (:use flow-gl.utils
        clojure.test))

(defn once [time start-time duration]
  (let [phase (min 1
                   (/ (- time start-time)
                      duration))]
    {:phase phase
     :sleep-time (if (< phase 1)
                   0
                   nil)}))

(defn repeat [time start-time cycle-time]
  {:phase (/ (mod (- time start-time)
                  cycle-time)
             cycle-time)
   :sleep-time 0})

(defn linear [phase from to]
  (+ from
     (* (- to from)
        phase )))

(defn call-animation [state key phaser phaser-arguments animation animation-arguments]
  (let [{:keys [phase sleep-time]} (apply phaser
                                          quad-gui/current-frame-time
                                          (or (key state)
                                              quad-gui/current-frame-time)
                                          phaser-arguments)]
    (quad-gui/set-wake-up sleep-time)
    (apply animation
           phase
           animation-arguments)))

(quad-gui/def-control other
  ([view-context control-channel]
     {})

  ([view-context state]
     (drawable/->Rectangle 100 100 (:color state))))

(quad-gui/def-control animation
  ([view-context control-channel]
     {:target-position :left
      :animation (System/currentTimeMillis)})

  ([view-context {:keys [target-position] :as state}]
     (let [value (float (call-animation state :animation
                                        once [2000]
                                        linear [0 1]))]
       (l/vertically (controls/text value)
                     (-> (other {:color [value 0 1 1]})
                         (quad-gui/add-mouse-event-handler-with-context
                          view-context (fn [state event]
                                         (case (:type event)
                                           :mouse-clicked (assoc state :animation (:time event))
                                           state))))))


     #_(let [left-position 0
             right-position 200
             x-position (call-animation view-context
                                        :move-animation
                                        linear-animation
                                        2000
                                        (case target-position
                                          :left right-positon
                                          :right left-position)
                                        (case target-position
                                          :left left-position
                                          :right right-positon))
             color (call-animation view-context
                                   :color-animation
                                   linear-animation
                                   0
                                   1)
             size (call-animation view-context
                                  :size-animation
                                  linear-animation
                                  500
                                  100
                                  200)]

         (-> (l/margin 0 0 0 x-position
                       (drawable/->Rectangle size size [1 1 1 1]))
             (quad-gui/add-mouse-event-handler-with-context
              view-context (fn [state event]
                             (assoc state :size-animation (:time event)
                                    state :move-animation (:time event))))))))


#_(debug/reset-log)
(defn start []
  (debug/with-log (debug/create-log)
    (.start (Thread. (fn []
                       (quad-gui/start-view #'create-animation #'animation-view))))))

(quad-gui/redraw-last-started-view)

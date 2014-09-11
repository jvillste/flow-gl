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

(defn once [runtime duration]
  (let [phase (min 1
                   (/ runtime
                      duration))]

    (quad-gui/set-wake-up (if (< phase 1)
                            0
                            nil))
    phase))

(defn repeat [runtime cycle-time]
  {:phase (/ (mod runtime
                  cycle-time)
             cycle-time)
   :sleep 0})

(defn linear [phase from to]
  (+ from
     (* (- to from)
        phase )))

(defn call-phaser [phaser & phaser-arguments]
  (let [{:keys [phase sleep-time]} (apply phaser
                                          phaser-arguments)]
    (quad-gui/set-wake-up sleep-time)
    phase))

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
     ))

(quad-gui/def-control animation
  ([view-context control-channel]
     {:target-position :left
      :animation-started (System/currentTimeMillis)
      :animation-running false})

  ([view-context {:keys [target-position] :as state}]
     (let [value (float (let [runtime (if (:animation-running state)
                                        (- quad-gui/current-frame-time
                                           (:animation-started state))
                                        (or (:animation-runtime state)
                                            0))
                              {:keys [phase sleep]} (repeat runtime 1000)]
                          (when (:animation-running state)
                            (quad-gui/set-wake-up sleep))
                          (linear phase 100 200)))]
       (layouts/->Preferred (-> (drawable/->Rectangle value value [(if (:animation-running state)
                                                                     0
                                                                     1) 1 1 1])
                                (quad-gui/add-mouse-event-handler-with-context
                                 view-context
                                 (fn [state event]
                                   (case (:type event)
                                     :mouse-clicked (if (:animation-running state)
                                                      (-> state
                                                          (assoc :animation-running false)
                                                          (assoc :animation-runtime (- (:time event)
                                                                                       (:animation-started state))))
                                                      (-> state
                                                          (assoc :animation-running true)
                                                          (assoc :animation-started  (- (:time event)
                                                                                        (or (:animation-runtime state)
                                                                                            0)))))
                                     state))))))))


#_(debug/reset-log)
(defn start []
  (debug/with-log (debug/create-log)
    (.start (Thread. (fn []
                       (quad-gui/start-view #'create-animation #'animation-view))))))

(quad-gui/redraw-last-started-view)

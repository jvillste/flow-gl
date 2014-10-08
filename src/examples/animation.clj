(ns examples.animation
  (:require [clojure.core.async :as async]
            [com.climate.claypoole :as claypoole]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.debug :as debug]
            [flow-gl.tools.debug-monitor :as debug-monitor]
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
    {:sleep (if (< phase 1)
              0
              nil)
     :phase phase}))

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
    (gui/set-wake-up sleep-time)
    phase))

(defn start-stoppable-animation [state key time]
  (-> state
      (assoc [key :running] true)
      (assoc [key :started] (- time
                               (or (get state [key :runtime])
                                   0)))))

(defn stop-stoppable-animation [state key time]
  (-> state
      (assoc [key :running] false)
      (assoc [key :runtime] (- time
                               (get state [key :started])))))

(defn toggle-stoppable-animation [state key time]
  (if (get state [key :running])
    (stop-stoppable-animation state key time)
    (start-stoppable-animation state key time)))

(defn stoppable-animation-runtime [state key]
  (if (get state [key :running])
    (- gui/current-frame-time
       (get state [key :started]))
    (or (get state [key :runtime])
        0)))

(defn stoppable-animation-running [state key]
  (get state [:animation :running]))

(defn stoppable-animation-phase [state key phaser & phaser-arguments]
  (let [{:keys [phase sleep]} (apply phaser
                                     (stoppable-animation-runtime state :animation)
                                     phaser-arguments)]
    (when (stoppable-animation-running state :animation)
      (gui/set-wake-up sleep))
    phase))


(gui/def-control animation
  ([view-context control-channel]
     {:target-position :left})

  ([view-context {:keys [target-position] :as state}]
     (let [value (-> (stoppable-animation-phase state :animation repeat 6000)
                     (linear 100 200)
                     (float))]
       (layouts/->Preferred (-> (drawable/->Rectangle value value [(if (stoppable-animation-running state :animation)
                                                                     0
                                                                     1) 1 1 1])
                                (gui/on-mouse-clicked view-context
                                                           (fn [state time]
                                                             (toggle-stoppable-animation state :animation time))))))))


#_(debug/reset-log)
(defn start []
  (debug-monitor/with-debug-monitor
    (.start (Thread. (fn []
                       (gui/start-view #'create-animation #'animation-view))))))

(gui/redraw-last-started-view)

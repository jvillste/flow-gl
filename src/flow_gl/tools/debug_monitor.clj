(ns flow-gl.tools.debug-monitor
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


(quad-gui/def-view debug-monitor-view [view-context {:keys [values]}]
  (apply l/vertically (for [key (keys values)]
                        (controls/text (str key " : " (get values key))))))

(defn create-debug-monitor [channel view-context control-channel]
  (async/go-loop []
                 (async/alt! control-channel ([_] (println "exiting debug monitor process"))
                             channel ([entry]
                                        (when (= :metric
                                                 (:type entry))
                                          (quad-gui/apply-to-state view-context
                                                                   assoc-in
                                                                   [:values (:key entry)]
                                                                   (:value entry)))
                                        (recur))))
  {:values {:foo 10}})

(def channel (async/chan))
#_(debug/reset-log)

(defn start-monitor [channel]
  (.start (Thread. (fn []
                     (quad-gui/start-view (partial create-debug-monitor channel)
                                          #'debug-monitor-view)))))

(defn start []
  (start-monitor channel))

(quad-gui/redraw-last-started-view)

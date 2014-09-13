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
  #_(drawable/->Path 2 [1 1 1 1] [0 0
                                100 100
                                110 10])

  (if (not (empty? (keys values)))
      (apply l/vertically (for [key (keys values)]
                            (let [value (get values key)]
                              (if (:ratio? value)
                                (l/horizontally (controls/text (str (name key) " : "))
                                                (drawable/->Rectangle (* (:value value) 200) 0  [1 0 0 1])
                                                (drawable/->Rectangle (* (- 1 (:value value)) 200) 0  [0 0 1 1]))
                                (controls/text (str (name key) " : " (:value value)))))))
      (drawable/->Empty 0 0)))

(defn create-debug-monitor [channel view-context control-channel]
  (async/go-loop []
                 (async/alt! control-channel ([_] (println "exiting debug monitor process"))
                             channel ([entry]
                                        (when (= :metric
                                                 (:type entry))
                                          (quad-gui/apply-to-state view-context
                                                                   assoc-in
                                                                   [:values (:key entry)]
                                                                   {:value (:value entry)
                                                                    :ratio? (:ratio? entry)}))
                                        (recur))))
  {})

#_(def channel (async/chan))

(defn start-monitor [channel]
  (.start (Thread. (fn []
                     (quad-gui/start-view (partial create-debug-monitor channel)
                                          #'debug-monitor-view)))))

(defn start-monitor-with-new-channel []
  (let [channel (async/chan)]
    (start-monitor channel)
    channel))

(defmacro with-debug-monitor [& body]
  `(debug/with-debug-channel (start-monitor-with-new-channel)
     ~@body))

(defn start []
  (with-debug-monitor
    (Thread/sleep 1000)
    (debug/set-metric :bar 1000)))

(quad-gui/redraw-last-started-view)

(ns flow-gl.tools.log
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

(defn create-blocks [log]
  (loop [open-blocks []
         open-block {:children []}
         entries log]
    (if-let [entry (first entries)]
      (case (:block entry)
        :start
        (let [new-block {:children []
                         :start-time (:time entry)
                         :message (:message entry)}]
          (recur (conj open-blocks open-block)
                 new-block
                 (rest entries)))

        :end
        (let [parent-block (last open-blocks)]
          (recur (pop open-blocks)
                 (update-in parent-block [:children] conj (assoc open-block
                                                                  :end-time (:time entry)))
                 (rest entries)))

        nil
        (recur open-blocks
               (update-in open-block [:children] conj {:start-time (:time entry)
                                                       :end-time (:time entry)
                                                       :message (:message entry)
                                                       :children []})
               (rest entries)))

      (:children open-block))))

(println (create-blocks [{:time 1
                          :message "1"}
                         {:time 2
                          :message "2"
                          :block :start}
                         {:time 3
                          :message "3"
                          :block :start}
                         {:time 4
                          :message "4"}
                         {:time 5
                          :message "3"
                          :block :end}
                         {:time 6
                          :message "2"
                          :block :end}]))

(defn color [message]
  (let [random (Random. (reduce + (map int (seq message))))]
    [(.nextFloat random)
     (.nextFloat random)
     (.nextFloat random)
     1]))

(quad-gui/def-control log-browser
  ([view-context control-channel]
     {:log (take 30 @debug/log)})

  ([view-context {:keys [log]}]
     (let [threads (reduce  conj #{} (map :thread log))]
       (apply l/horizontally (for [thread threads]
                               (apply l/vertically (for [{:keys [message time thread block]} (filter #(= (:thread %) thread) log)]
                                                     (layouts/->Box 3 [(drawable/->Rectangle 0 0 (color message))
                                                                       (controls/text (str message " : " block))])))))

       )))

#_(debug/reset-log)
(defn start []
  (.start (Thread. (fn []
                     (quad-gui/start-view #'create-log-browser #'log-browser-view)))))

(when-let [last-event-channel-atom @quad-gui/last-event-channel-atom]
  (async/put! last-event-channel-atom {:type :request-redraw}))

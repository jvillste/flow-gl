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

(def log [{:time 1
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
           :block :end}])

#_(println (create-blocks log))

(defn color [message]
  (let [random (Random. (reduce + (map int (seq message))))]
    [(.nextFloat random)
     (.nextFloat random)
     (.nextFloat random)
     1]))

(defn block-view [block depth height-factor]
  (let [width 34
        indent (* 0.2 width)]
    (-> (layouts/->Superimpose [(drawable/->Rectangle (- width (* indent depth))
                                                      (* height-factor (max 1 (- (:end-time block)
                                                                                 (:start-time block))))
                                                      (color (:message block)))

                                (assoc (apply l/vertically (loop [previous-child-end-time (:start-time block)
                                                                  children (:children block)
                                                                  child-block-views []]
                                                             (if-let [child (first children)]
                                                               (recur (:end-time child)
                                                                      (rest children)
                                                                      (conj child-block-views
                                                                            (l/margin (* height-factor (- (:start-time child)
                                                                                                          previous-child-end-time))
                                                                                      0
                                                                                      0
                                                                                      indent
                                                                                      (layouts/->Preferred (block-view child (inc depth) height-factor)))))
                                                               child-block-views)))
                                  :on-drag (fn [state x y]
                                             (println "on drag"  x y)
                                             (-> state
                                                 (update-in [:translate-x] + x)
                                                 (update-in [:translate-y] + y))))])
        (assoc :on-mouse-enter (fn [state]
                                 (assoc state
                                   :message (str (:message block) " : " (- (:end-time block) (:start-time block)))))
               ))))

(defn text-block-view [block depth]
  (let [indent 10]
    (apply l/vertically (concat [(controls/text (:message block))]
                                (for [child (:children block)]
                                  (l/margin 0
                                            0
                                            0
                                            indent
                                            (layouts/->Preferred (text-block-view child (inc depth)))))))))

#_(defn block-list [blocks]
    (apply l/vertically (for [block blocks]
                          ))
    (layouts/->Abso))

(quad-gui/def-control log-browser
  ([view-context control-channel]
     {:log log #_(take 100 @debug/log)
      :message ""
      :height-factor 10
      :translate-x 0
      :translate-y 0})

  ([view-context {:keys [log message height-factor translate-x translate-y]}]
     (let [threads (->> (reduce conj #{} (map :thread log))
                        (vec)
                        (sort-by (fn [thread-number] (-> (filter #(= (:thread %) thread-number) log)
                                                         (count))))
                        (reverse))]
       (l/vertically (controls/text message)
                     (layouts/->Translate translate-x translate-y
                                          (assoc (apply l/horizontally (for [thread threads]
                                                                         (let [blocks (-> (filter #(= (:thread %) thread) log)
                                                                                          (create-blocks))]
                                                                           (l/margin 0 0 0 2
                                                                                     (l/horizontally (apply l/vertically (for [block blocks]
                                                                                                                           (block-view block 0 height-factor)
                                                                                                                           ))
                                                                                                     #_(apply l/vertically (for [block blocks]
                                                                                                                             (text-block-view block 0))))))))
                                            :handle-mouse-event (fn [state event]
                                                                  (case (:type event)
                                                                    :mouse-wheel-moved (update-in state [:height-factor] + (:y-distance event))
                                                                    state))))))))

#_(def log @debug/log)
#_(debug/reset-log)
(defn start []
  (.start (Thread. (fn []
                     (quad-gui/start-view #'create-log-browser #'log-browser-view)))))

(when-let [last-event-channel-atom @quad-gui/last-event-channel-atom]
  (async/put! last-event-channel-atom {:type :request-redraw}))

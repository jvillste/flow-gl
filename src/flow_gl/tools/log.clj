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

(def test-log [{:time 1
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

(defn color [message]
  (let [random (Random. (reduce + (map int (seq message))))]
    [(.nextFloat random)
     (.nextFloat random)
     (.nextFloat random)
     1]))

(defn block-view [view-context block depth height-factor]
  (let [width 34
        indent (* 0.2 width)]
    (-> (layouts/->Superimpose [(drawable/->Rectangle (- width (* indent depth))
                                                      (* height-factor (max 1 (- (:end-time block)
                                                                                 (:start-time block))))
                                                      (color (:message block)))

                                (apply l/vertically (loop [previous-child-end-time (:start-time block)
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
                                                                               (layouts/->Preferred (block-view view-context child (inc depth) height-factor)))))
                                                        child-block-views)))])
        (quad-gui/add-mouse-event-handler-with-context view-context
                                                       (fn [state event]
                                                         (if (= (:type event)
                                                                :mouse-enter)
                                                           (assoc state
                                                             :message (str (:message block) " : " (- (:end-time block) (:start-time block))))
                                                           state))))))

(defn text-block-view [block depth]
  (let [indent 10]
    (apply l/vertically (concat [(controls/text (:message block))]
                                (for [child (:children block)]
                                  (l/margin 0
                                            0
                                            0
                                            indent
                                            (layouts/->Preferred (text-block-view child (inc depth)))))))))

(quad-gui/def-control scroll-pane
  ([view-context control-channel]
     {:x-translation 0
      :y-translation 0})

  ([view-context {:keys [content x-translation y-translation]}]
     (layouts/->Translate x-translation y-translation
                          (assoc content
                            :handle-mouse-event (fn [state event]
                                                  (case (:type event)
                                                    :mouse-pressed (assoc state
                                                                     :previous-drag-x (:x event)
                                                                     :previous-drag-y (:y event))
                                                    :mouse-released (assoc state
                                                                      :previous-drag-x nil
                                                                      :previous-drag-y nil)
                                                    :mouse-dragged (assoc state
                                                                     :x-translation (+ (:x-translation state)
                                                                                       (- (:x event)
                                                                                          (:previous-drag-x state)))
                                                                     :y-translation (+ (:y-translation state)
                                                                                       (- (:y event)
                                                                                          (:previous-drag-y state)))
                                                                     :previous-drag-x (:x event)
                                                                     :previous-drag-y (:y event))
                                                    state))))))

(quad-gui/def-control log-browser
  ([view-context control-channel]
     {:log log #_(take 100 @debug/log)
      :message ""
      :height-factor 10
      :translate-x 0
      :y-translation 0})

  ([view-context {:keys [log message height-factor translate-x translate-y]}]
     (let [threads (->> (reduce conj #{} (map :thread log))
                        (vec)
                        (sort-by (fn [thread-number] (-> (filter #(= (:thread %) thread-number) log)
                                                         (count))))
                        (reverse))]
       (l/vertically (controls/text message)
                     (scroll-pane :content
                                  (-> (apply l/horizontally (for [thread threads]
                                                              (let [blocks (-> (filter #(= (:thread %) thread) log)
                                                                               (create-blocks))]
                                                                (l/margin 0 0 0 2
                                                                          (l/horizontally (apply l/vertically (for [block blocks]
                                                                                                                (block-view view-context block 0 height-factor)
                                                                                                                ))
                                                                                          #_(apply l/vertically (for [block blocks]
                                                                                                                  (text-block-view block 0))))))))
                                      (quad-gui/add-mouse-event-handler-with-context
                                       view-context
                                       (fn [state event]

                                         (case (:type event)
                                           :mouse-wheel-moved (update-in state [:height-factor] + (:y-distance event))
                                           state)))) )
                     ))))

#_(def log @debug/log)
(def log test-log)
#_(debug/reset-log)
(defn start []
  (.start (Thread. (fn []
                     (quad-gui/start-view #'create-log-browser #'log-browser-view)))))

(when-let [last-event-channel-atom @quad-gui/last-event-channel-atom]
  (async/put! last-event-channel-atom {:type :request-redraw}))

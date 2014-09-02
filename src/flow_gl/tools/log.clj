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
         open-block {:children []
                     :start-time (:time (first log))
                     :end-time (:time (last log))}
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

      open-block)))

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


(def test-log [{:time 1
                :message "1"
                :block :start}
               {:time 2
                :message "1"
                :block :end}

               {:time 4
                :message "1"
                :block :start}
               {:time 5
                :message "1"
                :block :end}

               {:time 7
                :message "1"
                :block :start}
               {:time 8
                :message "1"
                :block :end}])

#_(println (create-blocks test-log))

(defn color [message]
  (let [random (Random. (reduce + (map int (seq message))))]
    [(.nextFloat random)
     (.nextFloat random)
     (.nextFloat random)
     1]))

(defn block-view [view-context block depth height-factor width]
  (let [indent (* 0.2 width)]
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
                                                                               (layouts/->Preferred (block-view view-context child (inc depth) height-factor width)))))
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
     (-> (layouts/->SizeDependent
          (fn [available-width available-height]
            {:width available-width
             :height available-height})

          (fn [state requested-width requested-height]
            (layouts/->Translate x-translation y-translation
                                 (content (max 0 (- x-translation))
                                          (max 0 (- y-translation))
                                          (max 0 (- requested-width
                                                    x-translation))
                                          (max 0 (- requested-height
                                                    y-translation))))))

         (quad-gui/add-mouse-event-handler-with-context view-context
                                                        (fn [state event]
                                                          (cond
                                                           (and (= (:type event) :mouse-wheel-moved)
                                                                (not (:control event)))
                                                           (-> state
                                                               (update-in [:x-translation] + (:x-distance event))
                                                               (update-in [:y-translation] + (:y-distance event)))

                                                           :default state))))))


#_(def log @debug/log)
(def log test-log)

#_(defn filter-blocks-by-rectangle [blocks start-time x1 y1 x2 y2]
    (loop [blocks blocks
           filtered-blocks []]
      (if-let [block (first blocks)]
        (recur (rest blocks)
               (if []))
        filtered-blocks)))

(defn print-and-return [message value]
  (println message value)
  value)

(quad-gui/def-control log-browser
  ([view-context control-channel]
     {:thread-blocks (let [threads (->> (reduce conj #{} (map :thread log))
                                        (vec)
                                        (sort-by (fn [thread-number] (-> (filter #(= (:thread %) thread-number) log)
                                                                         (count))))
                                        (reverse))]
                       (for [thread threads]
                         (create-blocks (filter #(= (:thread %) thread) log))))
      :message ""
      :height-factor 10
      :thread-width 200
      :translate-x 0
      :y-translation 0})

  ([view-context {:keys [thread-blocks message height-factor thread-width translate-x translate-y]}]
     (layouts/->FloatTop (controls/text message)
                         (-> (scroll-pane :content
                                          (fn [x1 y1 x2 y2]
                                            (apply l/horizontally (for [root-block thread-blocks]
                                                                    (let [blocks (:children root-block)
                                                                          first-block-start-time (:start-time root-block)
                                                                          first-visible-time (+ first-block-start-time
                                                                                                (/ y1 height-factor))
                                                                          last-visible-time (+ first-block-start-time
                                                                                               (/ y2 height-factor))
                                                                          visible-blocks (filter (fn [{:keys [start-time end-time]}]
                                                                                                   (and (< start-time
                                                                                                           last-visible-time)
                                                                                                        (> end-time
                                                                                                           first-visible-time)))
                                                                                                 blocks)]
                                                                      (if (empty? visible-blocks)
                                                                        (drawable/->Empty thread-width 0)
                                                                        (let [root-block (assoc root-block
                                                                                           :children visible-blocks
                                                                                           :start-time (:start-time (first visible-blocks))
                                                                                           :end-time (:end-time (last visible-blocks)))]
                                                                          (l/margin (* height-factor (:start-time root-block)) 0 0 2
                                                                                    (block-view view-context root-block 0 height-factor thread-width)))))))))
                             (quad-gui/add-mouse-event-handler-with-context
                              view-context
                              (fn [state event]
                                (cond
                                 (and (= (:type event) :mouse-wheel-moved)
                                      (:control event))
                                 (update-in state [:height-factor] + (* 0.5 (:y-distance event)))

                                 :default state)))))))


#_(layouts/->SizeDependent
   (fn [available-width available-height]
     {:width available-width
      :height available-height})

   (fn [state requested-width requested-height]
     (apply l/horizontally (loop [threads threads]
                             (let [blocks (-> (filter #(= (:thread %) thread) log)
                                              (create-blocks))]
                               (l/margin 0 0 0 2
                                         (l/horizontally (apply l/vertically (for [block blocks]
                                                                               (block-view view-context block 0 height-factor)))
                                                         #_(apply l/vertically (for [block blocks]
                                                                                 (text-block-view block 0))))))))))

#_(debug/reset-log)
(defn start []
  (.start (Thread. (fn []
                     (quad-gui/start-view #'create-log-browser #'log-browser-view)))))

(when-let [last-event-channel-atom @quad-gui/last-event-channel-atom]
  (async/put! last-event-channel-atom {:type :request-redraw}))

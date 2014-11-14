(ns flow-gl.tools.log
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
    [(.nextInt random 255)
     (.nextInt random 255)
     (.nextInt random 255)
     255]))

(defn block-view [view-context block depth y-scale width]
  (let [indent (* 0.2 width)]
    (-> (layouts/->Superimpose [(drawable/->Rectangle (- width (* indent depth))
                                                      (* y-scale (max 0.1 (- (:end-time block)
                                                                             (:start-time block))))
                                                      (color (:message block)))

                                (apply l/vertically (loop [previous-child-end-time (:start-time block)
                                                           children (:children block)
                                                           child-block-views []]
                                                      (if-let [child (first children)]
                                                        (recur (:end-time child)
                                                               (rest children)
                                                               (conj child-block-views
                                                                     (l/margin (* y-scale (- (:start-time child)
                                                                                             previous-child-end-time))
                                                                               0
                                                                               0
                                                                               indent
                                                                               (layouts/->Preferred (block-view view-context child (inc depth) y-scale width)))))
                                                        child-block-views)))])
        (gui/add-mouse-event-handler-with-context view-context
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

(gui/def-control scroll-pane
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

         (gui/add-mouse-event-handler-with-context view-context
                                                   (fn [state event]
                                                     (cond
                                                      (and (= (:type event) :mouse-wheel-moved)
                                                           (not (:control event)))
                                                      (-> state
                                                          (update-in [:x-translation] + (:x-distance event))
                                                          (update-in [:y-translation] + (:y-distance event)))

                                                      :default state))))))

#_(debug/reset-log)
#_(def log @debug/log)
(def log test-log)
#_(def log @examples.photos/log)

(def thread-blocks (let [start-time (:time (first log))
                         log (map (fn [entry]
                                    (update-in entry [:time] - start-time))
                                  log)
                         threads (->> (reduce conj #{} (map :thread log))
                                      (vec)
                                      (sort-by (fn [thread-number] (-> (filter #(= (:thread %) thread-number) log)
                                                                       (count))))
                                      (reverse))]
                     (for [thread threads]
                       (create-blocks (filter #(= (:thread %) thread) log)))))


(defn print-and-return [message value]
  (println message value)
  value)

(defn new-y-translation [mouse-y old-y-scale new-y-scale y-translation]
  (let [mouse-distance-to-top (- mouse-y y-translation)]
    (+ y-translation
       (* (/ mouse-distance-to-top old-y-scale)
          (- old-y-scale new-y-scale)))))

(gui/def-control log-browser
  ([view-context control-channel]
     {:thread-blocks thread-blocks
      :message ""
      :y-scale 1
      :thread-width 100
      :translate-x 0
      :y-translation 0})

  ([view-context {:keys [thread-blocks message y-scale thread-width translate-x translate-y]}]
     (layouts/->FloatTop (controls/text message)
                         #_(let [y1 0 y2 200]
                             (apply l/horizontally (for [root-block thread-blocks]
                                                     (let [blocks (:children root-block)
                                                           first-visible-time (/ y1 y-scale)
                                                           last-visible-time (/ y2 y-scale)
                                                           visible-blocks (filter (fn [{:keys [start-time end-time]}]
                                                                                    (and (< start-time
                                                                                            last-visible-time)
                                                                                         (> end-time
                                                                                            first-visible-time)))
                                                                                  blocks)]
                                                       (let [root-block (assoc root-block
                                                                          :children visible-blocks
                                                                          :start-time (max first-visible-time
                                                                                           (:start-time (first blocks)))
                                                                          :end-time (min last-visible-time
                                                                                         (:end-time (last blocks))))]

                                                         (l/margin (* y-scale (:start-time root-block)) 0 0 2
                                                                   (block-view view-context root-block 0 y-scale thread-width)))))))

                         (-> (scroll-pane :scroll-pane
                                          {:content
                                           (fn [x1 y1 x2 y2]
                                             (apply l/horizontally (for [root-block thread-blocks]
                                                                     (let [blocks (:children root-block)
                                                                           first-visible-time (/ y1 y-scale)
                                                                           last-visible-time (/ y2 y-scale)
                                                                           visible-blocks (filter (fn [{:keys [start-time end-time]}]
                                                                                                    (and (< start-time
                                                                                                            last-visible-time)
                                                                                                         (> end-time
                                                                                                            first-visible-time)))
                                                                                                  blocks)]
                                                                       (let [root-block (assoc root-block
                                                                                          :children visible-blocks
                                                                                          :start-time (max first-visible-time
                                                                                                           (:start-time (first blocks)))
                                                                                          :end-time (min last-visible-time
                                                                                                         (:end-time (last blocks))))]

                                                                         (l/margin (* y-scale (:start-time root-block)) 0 0 2
                                                                                   (block-view view-context root-block 0 y-scale thread-width)))))))})
                             (gui/add-mouse-event-handler-with-context
                              view-context
                              (fn [state event]
                                (cond
                                 (and (= (:type event) :mouse-wheel-moved)
                                      (:control event))
                                 (let [new-y-scale (+ (:y-scale state)
                                                      (* (:y-scale state)
                                                         0.01
                                                         (:y-distance event)))]

                                   (-> state
                                       (assoc :y-scale new-y-scale)
                                       (update-in [:thread-width] + (* 0.1 (:x-distance event)))
                                       (update-in [:child-states :scroll-pane :y-translation] (fn [y-translation]
                                                                                                (new-y-translation (- (:y event) 20)
                                                                                                                   (:y-scale state)
                                                                                                                   new-y-scale
                                                                                                                   y-translation)))))




                                 :default state)))))))




#_(debug/reset-log)
(defn start []
  (.start (Thread. (fn []
                     (gui/start-view #'create-log-browser #'log-browser-view)))))

(when-let [last-event-channel-atom @gui/last-event-channel-atom]
  (async/put! last-event-channel-atom {:type :request-redraw}))

(ns flow-gl.tools.profiler
  (:require [clojure.core.async :as async]
            [com.climate.claypoole :as claypoole]
            (flow-gl.opengl.jogl [quad :as quad])
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l]
                         [transformer :as transformer])
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

(defn create-state []
  {:total-times {}
   :total-counts {}
   :event-counts {}
   :open-blocks {}})

(defn add-entry [profiler entry]
  (case (:block entry)
    :start
    (update-in profiler [:open-blocks] assoc (:block-id entry) entry)

    :end
    (if-let [category (get-in profiler [:open-blocks (:block-id entry) :category])]
      (-> profiler
          (update-in [:total-times category] (fn [old-time]
                                               (+ (or old-time 0)
                                                  (- (:time entry)
                                                     (get-in profiler
                                                             [:open-blocks
                                                              (:block-id entry)
                                                              :time])))))

          (update-in [:total-counts category] (fnil inc 0))

          (update-in [:open-blocks] dissoc (:block-id entry)))
      profiler)

    nil
    (case (:type entry)
      :event (update-in profiler [:event-counts (:category entry)] (fnil inc 0))
      :reset (create-state)
      profiler)))

;; UI
(defn text [value]
  (drawable/->Text (str value)
                   (font/create "LiberationSans-Regular.ttf" 15)
                   [255 255 255 255]))

(defn text-cell [value]
  (l/margin 1 2 1 2 (text value)))

(defn profiler-view [view-context {:keys [profiler]}]
  (let [rows (->> (map (fn [[category time]]
                         {:category category
                          :time time})
                       (:total-times profiler))
                  (filter #(not= (:category %) :total))
                  (sort-by :time)
                  (reverse))
        %-of-total (fn [time] (format "%.2f" (float (if-let [total (get-in profiler [:total-times :total])]
                                                      (float (* 100 (/ time total)))
                                                      0))))]
    (l/vertically (-> (text-cell "reset")
                      (gui/on-mouse-event-with-view-context :mouse-clicked
                                          view-context
                                          (fn [state event]
                                            (async/put! (:channel state) {:type :reset})
                                            state)))
                  (l/margin 10 0 0 0
                            (layouts/grid (concat [[(text-cell "Category")
                                                    (text-cell "% of total")
                                                    (text-cell "Total time")
                                                    (text-cell "Total count")
                                                    (text-cell "Average")
                                                    ]]
                                                  (for-all [{:keys [category time]} rows]

                                                           [(text-cell category)
                                                            (text-cell (%-of-total time))
                                                            (text-cell time)
                                                            (text-cell (get-in profiler [:total-counts category]))
                                                            (text-cell (int (/ time (get-in profiler [:total-counts category]))))]))))


                  (text-cell (str "rest: "
                                  (if-let [total (get-in profiler [:total-times :total])]
                                    (->> rows
                                         (map :time)
                                         (reduce +)
                                         (- total)
                                         %-of-total)
                                    "")
                                  " %"))

                  (l/margin 10 0 0 0
                            (layouts/grid (concat [[(text-cell "Category")
                                                    (text-cell "Count")]]
                                                  (for-all [[category count] (->> (:event-counts profiler)
                                                                                  (sort-by #(str (first %))))]

                                                           [(text-cell category)
                                                            (text-cell count)]))))




                  #_(controls/text profiler))))

(defn create-profiler-control [channel]
  (fn [view-context]
    (let [profiler-channel (async/chan)
          throttled-channel (csp/throttle-at-constant-rate profiler-channel 500)]

      (async/go-loop [profiler (create-state)]
        (if-let [entry (async/<! channel)]
          (let [new-profiler (add-entry profiler entry)]
            (async/>! profiler-channel new-profiler)
            (recur new-profiler))
          (async/close! profiler-channel)))

      (async/go-loop []
        (when-let [new-profiler (async/<! throttled-channel)]
          (gui/apply-to-state view-context assoc-in [:profiler] new-profiler)
          (recur))))

    {:local-state {:profiler (create-state)
                   :channel channel}
     :view profiler-view}))

;; API

(defmacro with-profiler [& body]
  `(let [channel# (async/chan 50)]
     (.start (Thread. (fn []
                        (gui/start-control (create-profiler-control channel#)))))
     (async/>!! channel# {})
     (debug/with-debug-channel channel# ~@body)
     (async/close! channel#)))


;; Tests

(defn start []
  (flow-gl.gui.cache/with-cache-disabled
    (with-profiler
      (debug/debug-timed-and-return :total (do (debug/debug-timed-and-return :foo (Thread/sleep 1000))

                                               (Thread/sleep 100)
                                               (debug/add-event :event)
                                               (debug/debug-timed-and-return :foo (Thread/sleep 2000))
                                               (Thread/sleep 100)
                                               (debug/add-event :event))))))


(gui/redraw-last-started-view)

#_(-> (create-state)
      (add-entry {:time 1
                  :block-id 1
                  :category :foo
                  :block :start})
      (add-entry {:time 2
                  :block-id 1
                  :block :end}))

#_(def channel (async/chan 50))

#_(do
    (async/go-loop []
      (let [entries (csp/drain channel nil)]
        (println "got" entries)
        (Thread/sleep 100)
        (when entries
          (recur))))

    (async/put! channel 1)
    (async/put! channel 2)
    (async/put! channel 3)

    (Thread/sleep 2000)
    (async/close! channel))

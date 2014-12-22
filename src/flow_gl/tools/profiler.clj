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
   :open-blocks {}})

(defn add-entry [profiler entry]
  (case (:block entry)
    :start
    (update-in profiler [:open-blocks] assoc (:block-id entry) entry)

    :end
    (let [category (get-in profiler [:open-blocks (:block-id entry) :category])]
      (-> profiler
          (update-in [:total-times category] (fn [old-time]
                                               (+ (or old-time 0)
                                                  (- (:time entry)
                                                     (get-in profiler
                                                             [:open-blocks
                                                              (:block-id entry)
                                                              :time])))))

          (update-in [:total-counts category] (fn [old-count] (inc (or old-count 0))))

          (update-in [:open-blocks] dissoc (:block-id entry))))

    nil
    profiler))

;; UI

(defn profiler-view [view-context {:keys [profiler]}]
  (l/vertically (controls/text (str profiler) [255 0 200 255]) ))

(defn create-profiler-control [channel]
  (fn [view-context]
    (let [profiler (create-state)]

      (async/go-loop [profiler profiler]
        (gui/apply-to-state view-context assoc :profiler profiler)
        (let [entry (async/<! channel)]
          (when entry
            (recur (add-entry profiler entry)))))

      {:profiler profiler
       :view #'profiler-view})))

;; API

(defmacro with-profiler [& body]
  `(let [channel# (async/chan)]
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
                                               (debug/debug-timed-and-return :foo (Thread/sleep 2000)))))))


(gui/redraw-last-started-view)

#_(-> (create-state)
      (add-entry {:time 1
                  :block-id 1
                  :category :foo
                  :block :start})
      (add-entry {:time 2
                  :block-id 1
                  :block :end}))

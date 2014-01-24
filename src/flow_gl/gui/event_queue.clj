(ns flow-gl.gui.event-queue
  (:require [flow-gl.debug :as debug]))

(defn create [] (java.util.concurrent.LinkedBlockingQueue.))

(defn dequeue-events [event-queue]
  (loop [events (list)]
    (if (.peek event-queue)
      (do (debug/do-debug :events "deque event " (.peek event-queue))
          (recur (conj events (.take event-queue))))
      events)))

(defn dequeue-events-or-wait [event-queue]
  (if (not (.peek event-queue))
    (list (.take event-queue))
    (dequeue-events event-queue)))

(defn dequeue-event-or-wait [event-queue]
  (.take event-queue))

(defn add-event [event-queue event]
  (debug/do-debug :events "add event " event)
  (.put event-queue event))


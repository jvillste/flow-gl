(ns flow-gl.dependable-dataflow
  (:require [clojure.core.async :as async]
            [flow-gl.multimap :as multimap]
            [flow-gl.dataflow.dataflow :as dataflow]))

(defn create []
  {::dependants {}
   ::need-to-be-notified #{}
   ::definition-channel (async/chan 10)})

(defn dispose [dataflow]
  (async/close! (::definition-channel dataflow)))

(defn start [dataflow-atom]
  (async/go (loop [definition (async/<! (::definition-channel @dataflow-atom))]
              (when definition
                (let [[cell function] definition]
                  (swap! dataflow-atom dataflow/define cell function)
                  (recur (async/<! (::definition-channel @dataflow-atom))))))))

(defn add-dependant [dataflow notify-channel cell]
  (update-in dataflow [::dependants] multimap/add cell notify-channel))

(defn remove-dependant [dataflow notify-channel cell]
  (update-in dataflow [::dependants] multimap/del cell notify-channel))

(defn notify-changes [dataflow-atom cell]
  (let [dataflow (swap! dataflow-atom
                        (fn [dataflow]
                          (assoc dataflow
                            ::to-be-notified (::need-to-be-notified dataflow)
                            ::need-to-be-notified #{})))]

    (doseq [cell (::to-be-notified dataflow)
            channel (get-in dataflow [::dependants cell])]
      (async/do (async/>! channel cell)))))

(defn schedule-for-notification [dataflow cell]
  (update-in dataflow [::need-to-be-notified] conj cell))

(defn update-cell [base-update-cell dataflow cell]
  (let [new-dataflow (base-update-cell dataflow cell)]
    (if (not (= (dataflow/unlogged-get-value dataflow cell)
                (dataflow/unlogged-get-value new-dataflow cell)))
      (schedule-for-notification new-dataflow cell)
      new-dataflow)))

(defn schedule-definition [dataflow cell function]
  (async/go (async/>! (::definition-channel dataflow) [cel function])))


(def dataflow-implementation {:define define
                              :undefine undefine
                              :get-value get-value
                              :unlogged-get-value unlogged-get-value
                              :is-defined? is-defined?
                              :update-cell update-cell
                              :propagate-changes propagate-changes})

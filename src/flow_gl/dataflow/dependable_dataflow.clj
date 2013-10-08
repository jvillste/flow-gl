(ns flow-gl.dataflow.dependable-dataflow
  (:require [clojure.core.async :as async]
            [flow-gl.multimap :as multimap]
            [flow-gl.dataflow.dataflow :as dataflow]))

(defn create []
  {::notification-channel-dependencies {}
   ::changes-to-be-notified #{}})

(defn set-notification-channel-dependencies [dataflow notification-channel dependencies]
  (update-in dataflow [::notification-channel-dependencies notification-channel] assoc dependencies))

(defn dependent-notification-channels [dataflow cell]
  (filter #(contains? (get-in dataflow [::notification-channel-dependencies %])
                      cell)
          (keys (::notification-channel-dependencies dataflow))))

(defn notify-dependents [dataflow-atom]
  (let [dataflow (swap! dataflow-atom
                        (fn [dataflow]
                          (assoc dataflow
                            ::cnahges-to-be-notified-now (::changes-to-be-notified dataflow)
                            ::changes-to-be-notified #{})))]

    (doseq [cell (::cnahges-to-be-notified-now dataflow)
            channel (dependent-notification-channels dataflow cell)]
      (async/go (async/>! channel cell)))))

(defn schedule-for-notification [dataflow cell]
  (update-in dataflow [::changes-to-be-notified] conj cell))

(defn update-cell [base-update-cell dataflow cell]
  (let [new-dataflow (base-update-cell dataflow cell)]
    (if (not (= (dataflow/unlogged-get-value dataflow cell)
                (dataflow/unlogged-get-value new-dataflow cell)))
      (schedule-for-notification new-dataflow cell)
      new-dataflow)))

(def dataflow-implementation {:update-cell update-cell})

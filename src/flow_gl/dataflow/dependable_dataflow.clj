(ns flow-gl.dataflow.dependable-dataflow
  (:require [clojure.core.async :as async]
            [flow-gl.multimap :as multimap]
            [flow-gl.dataflow.dataflow :as dataflow]
            [flow-gl.dataflow.base-dataflow :as base-dataflow])
  (:use clojure.test))



(defn set-notification-channel-dependencies [dataflow notification-channel dependencies]
  (flow-gl.debug/debug :dataflow "setting notification channel dependencies: "  notification-channel " = " dependencies)
  (if (empty? dependencies)
    (update-in  dataflow [::notification-channel-dependencies] dissoc notification-channel)
    (assoc-in dataflow [::notification-channel-dependencies notification-channel] dependencies)))

(defn dependent-notification-channels [dataflow cell]
  (filter #(some #{cell} (get-in dataflow [::notification-channel-dependencies %]))
          (keys (::notification-channel-dependencies dataflow))))

(deftest dependent-notification-channels-test
  (is (= (dependent-notification-channels (set-notification-channel-dependencies {} :channel [:cell-1 :cell-2])
                                          :cell-1)
         '(:channel))))

(defn notify-dependents [dataflow-atom]
  (let [dataflow (swap! dataflow-atom
                        (fn [dataflow]
                          (assoc dataflow
                            ::changes-to-be-notified-now (::changes-to-be-notified dataflow)
                            ::changes-to-be-notified #{})))]
    (flow-gl.debug/debug :dataflow "notifiying: " (::changes-to-be-notified-now dataflow) " : " (::notification-channel-dependencies dataflow))
    (doseq [cell (::changes-to-be-notified-now dataflow)
            channel (dependent-notification-channels dataflow cell)]
      (flow-gl.debug/debug :dataflow "notifiying cell " cell " " channel)

      (async/>!! channel cell))))

(defn schedule-for-notification [dataflow cell]
  (update-in dataflow [::changes-to-be-notified] conj cell))

(defn update-cell [base-update-cell dataflow cell]
  (let [new-dataflow (base-update-cell dataflow cell)]
    (if (not (= (dataflow/unlogged-get-value dataflow cell)
                (dataflow/unlogged-get-value new-dataflow cell)))
      (schedule-for-notification new-dataflow cell)
      new-dataflow)))

;; PROTOCOL

(def dataflow-implementation {:update-cell update-cell})

(defrecord DependableDataflow [])

(extend DependableDataflow
  dataflow/Dataflow
  (merge base-dataflow/dataflow-implementation
         {:update-cell (partial update-cell base-dataflow/update-cell)}))


;; CREATE

(defn initialize []
  {::notification-channel-dependencies {}
   ::changes-to-be-notified #{}})

(defn create []
  (-> (base-dataflow/create)
      (merge (initialize))
      (map->DependableDataflow)))

#_(run-tests)

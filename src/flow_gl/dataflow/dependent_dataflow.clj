(ns flow-gl.dependent-dataflow
  (:require [flow-gl.debug :as debug]
            [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.dataflow :as dataflow])
  (:use clojure.test))

(comment
  (let [common-dataflow (create-dataflow (create-file-storage "file.db"))
        view-dataflow (instantiate-traits [BaseDataflow {}]
                                          [HierarchialDataflow]
                                          [DependentDataflow common-dataflow])
        view (drawable/text (dataflow/get-value (dependent-dataflow/remote-cell-key :common :text)))
        handle-events (fn [state event]
                        )]
    (application/start view-dataflow
                       view)))

(defn create [& remote-dataflows]
  {::remote-dataflows (apply hash-map remote-dataflows)})

(defn add-remote-dataflow [dataflow key remote-dataflow]
  (assoc-in dataflow [::remote-dataflows key] remote-dataflow))

(defn get-remote-value [dataflow remote-dataflow cell]
  (dataflow/get-value (get-in dataflow [::remote-dataflows remote-dataflow])
                      cell))



(defn update-cell [dataflow cell]
  )

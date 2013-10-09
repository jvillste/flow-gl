(ns flow-gl.dataflow.dependent-dataflow
  (:require [clojure.core.async :as async]
            [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.dataflow :as dataflow]
            [flow-gl.dataflow.dependable-dataflow :as dependable-dataflow])
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


(defn start [dataflow-atom]
  )

(defn add-remote-dataflow [dataflow-atom remote-dataflow-key remote-dataflow-atom]
  (let [notification-channel (async/chan 10)]
    (swap! dataflow-atom (fn [dataflow]
                           (-> dataflow
                               (assoc-in [::remote-dataflow-atoms remote-dataflow-key] remote-dataflow-atom)
                               (assoc-in [::notification-channels remote-dataflow-key] notification-channel))))

    (async/go (loop [changed-cell (async/<! notification-channel)]
                (when changed-cell
                  (swap! dataflow-atom dataflow/declare-changed {::type ::remote-dependency
                                                                 ::remote-dataflow remote-dataflow-key
                                                                 ::cell changed-cell})
                  (recur (async/<! notification-channel)))))))


(defn remote-dataflow-dependents [dataflow remote-dataflow]
  (filter #(-> (get-in dataflow [::remote-dependencies %])
               (contains? remote-dataflow))
          (keys (::remote-dependencies dataflow))))

(deftest remote-dataflow-dependents-test
  (is (= (remote-dataflow-dependents {:flow-gl.dataflow.dependent-dataflow/remote-dependencies
                                      {:dependent
                                       {:remote-2 '(:target-cell-1),
                                        :remote-1 '(:target-cell-1 :target-cell-2)},
                                       :dependent-2 {:remote-2 '(:target-cell-1)}},
                                      :flow-gl.dataflow.dependent-dataflow/remote-dataflows-to-be-refreshed
                                      #{:remote-2 :remote-1 :remeote-3}}

                                     :remote-2)
         '(:dependent :dependent-2))))

(defn get-and-reset [atom key]
  (let [now-key (keyword (str (name key) "-now"))]
    (now-key (swap! atom (fn [value]
                           (assoc value now-key (key value)))))))

(defn refresh-remote-dataflow-dependencies [dataflow]
  (doseq [remote-dataflow (get-and-reset dataflow :remote-dataflows-to-be-refreshed)]
    (swap! (get-in dataflow [::remote-dataflow-atoms remote-dataflow])
           dependable-dataflow/set-notification-channel-dependencies
           (get-in dataflow [::notification-channels remote-dataflow])
           (remote-dataflow-dependents dataflow remote-dataflow))))

(defn map-vals [m f]
  (zipmap (keys m) (map f (vals m))))

(defn set-dependencies [base-set-dependencies dataflow dependent dependencies]
  (let [remote-dependencies (-> (->> dependencies
                                     (filter #(= (::type %)
                                                 ::remote-dependency))
                                     (group-by ::remote-dataflow))
                                (map-vals #(map ::cell %)))]
    (-> dataflow
        (assoc-in [::remote-dependencies dependent] remote-dependencies)
        (update-in [::remote-dataflows-with-changed-dependencies] into (keys remote-dependencies))
        (base-set-dependencies dependent dependencies))))

(deftest set-dependencies-test
  (is (= (set-dependencies (fn [dataflow _ _] dataflow)

                           {::remote-dataflows-with-changed-dependencies #{:remeote-3}
                            ::remote-dependencies {:dependent-2 {:remote-2 '(:target-cell-1)}}}

                           :dependent
                           [{::type ::remote-dependency
                             ::cell :target-cell-1
                             ::remote-dataflow :remote-1}
                            {::type ::remote-dependency
                             ::cell :target-cell-2
                             ::remote-dataflow :remote-1}
                            {::type ::remote-dependency
                             ::cell :target-cell-1
                             ::remote-dataflow :remote-2}])

         {:flow-gl.dataflow.dependent-dataflow/remote-dependencies
          {:dependent
           {:remote-2 '(:target-cell-1),
            :remote-1 '(:target-cell-1 :target-cell-2)},
           :dependent-2 {:remote-2 '(:target-cell-1)}},
          :flow-gl.dataflow.dependent-dataflow/remote-dataflows-to-be-refreshed
          #{:remote-2 :remote-1 :remeote-3}})))

(defn snapshot-remote-dataflows [dataflow]
  (assoc dataflow
    ::remote-dataflows (zipmap (keys (::remote-dataflow-atoms dataflow))
                               (map deref (vals (::remote-dataflow-atoms dataflow))))))

(defn get-remote-value [dataflow remote-dataflow cell]
  (logged-access/add-read {::type ::remote-dependency
                           ::remote-dataflow remote-dataflow
                           ::cell cell} [remote-dataflow cell])
  (dataflow/unlogged-get-value (get-in dataflow [::remote-dataflows remote-dataflow])
                               cell))

(defn undefine [base-undefine dataflow cell]
  (-> dataflow
      (update-in [::remote-dataflows-with-changed-dependencies] into (keys (get-in dataflow [::remote-dependencies cell])))
      (update-in [::remote-dependencies] disj cell)
      (base-undefine)))






;; PROTOCOL

(defrecord DependentDataflow [])

(extend DependentDataflow
  dataflow/Dataflow
  (merge base-dataflow/dataflow-implementation
         {:undefine (partial undefine base-dataflow/undefine)
          :set-dependencies (partial set-dependencies base-dataflow/set-dependencies)}))


;; CREATE

(defn initialize []
  {::remote-dataflow-atoms {}
   ::notification-channels {}
   ::remote-dependencies {}
   ::new-remote-dependencies {}
   ::remote-dataflows-with-changed-dependencies #{}})

(defn create [storage]
  (-> (base-dataflow/create storage)
      (merge (initialize))
      (map->DependentDataflow)))

(deftest dependent-dataflow-test
  (let [dependable-dataflow-atom (atom (dependable-dataflow/create {}))
        dependent-dataflow-atom (atom (create {}))])

  )

(run-tests)

(comment
  (db :text "foo")
  (editor :text [:db :text])
  )

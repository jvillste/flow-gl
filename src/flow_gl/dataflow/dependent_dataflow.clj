(ns flow-gl.dataflow.dependent-dataflow
  (:require [clojure.core.async :as async]
            [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            [flow-gl.utils :as utils]
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


(defn add-remote-dataflow [dataflow-atom remote-dataflow-key remote-dataflow-atom]
  (let [notification-channel (async/chan 10)]
    (swap! dataflow-atom (fn [dataflow]
                           (-> dataflow
                               (assoc-in [::remote-dataflow-atoms remote-dataflow-key] remote-dataflow-atom)
                               (assoc-in [::notification-channels remote-dataflow-key] notification-channel))))

    (async/go (loop [changed-cell (async/<! notification-channel)]
                (flow-gl.debug/debug :dataflow "got notification " changed-cell)
                (when changed-cell
                  (swap! dataflow-atom dataflow/declare-changed {::type ::remote-dependency
                                                                 ::remote-dataflow remote-dataflow-key
                                                                 ::cell changed-cell})
                  (recur (async/<! notification-channel)))))))


(defn dispose [dataflow]
  (println "disposing")
  (doseq [[remote-dataflow-key remote-dataflow-atom] (::remote-dataflow-atoms dataflow)]
    (swap! remote-dataflow-atom
           dependable-dataflow/set-notification-channel-dependencies
           (get-in dataflow [::notification-channels remote-dataflow-key])
           []))

  (doseq [notification-channel (vals (::notification-channels dataflow))]
    (async/close! notification-channel)))

(defn remote-dataflow-dependents [dataflow remote-dataflow]
  (filter #(-> (get-in dataflow [::remote-dependencies %])
               (contains? remote-dataflow))
          (keys (::remote-dependencies dataflow))))

(deftest remote-dataflow-dependents-test
  (is (= (remote-dataflow-dependents {:flow-gl.dataflow.dependent-dataflow/remote-dependencies
                                      {:dependent
                                       {:remote-2 '(:target-cell-1),
                                        :remote-1 '(:target-cell-1 :target-cell-2)},
                                       :dependent-2 {:remote-2 '(:target-cell-1)}}}

                                     :remote-2)
         '(:dependent :dependent-2))))


(defn remote-dataflow-dependencies [dataflow remote-dataflow]
  (->> (vals (::remote-dependencies dataflow))
      (mapcat remote-dataflow)
      (filter (complement nil?)) 
      (apply hash-set)))

(deftest remote-dataflow-dependencies-test
  (is (= (remote-dataflow-dependencies {:flow-gl.dataflow.dependent-dataflow/remote-dependencies
                                      {:dependent
                                       {:remote-2 '(:target-cell-1),
                                        :remote-1 '(:target-cell-1 :target-cell-2)},

                                       :dependent-2 {:remote-2 '(:target-cell-1 :target-cell-3)}}}

                                     :remote-2)

        #{:target-cell-1 :target-cell-3})))

(defn refresh-remote-dataflow-dependencies [dataflow-atom]
  (let [dataflow (utils/get-and-reset dataflow-atom ::remote-dataflows-with-changed-dependencies #{})]
    (flow-gl.debug/debug :dataflow "refreshing remote dataflow dependencies " (::remote-dataflows-with-changed-dependencies-now dataflow))
    (doseq [remote-dataflow (::remote-dataflows-with-changed-dependencies-now dataflow)]
      (flow-gl.debug/debug :dataflow "refreshing " remote-dataflow)
      (swap! (get-in dataflow [::remote-dataflow-atoms remote-dataflow])
             dependable-dataflow/set-notification-channel-dependencies
             (get-in dataflow [::notification-channels remote-dataflow])
             (remote-dataflow-dependencies dataflow remote-dataflow)))))



(defn set-dependencies [base-set-dependencies dataflow dependent dependencies]
  (flow-gl.debug/debug :dataflow "setting dependencies " dependent " : " dependencies)
  (let [remote-dependencies (-> (->> dependencies
                                     (filter #(= (::type %)
                                                 ::remote-dependency))
                                     (group-by ::remote-dataflow))
                                (utils/map-vals #(map ::cell %)))]
    (flow-gl.debug/debug :dataflow "setting remote dependencies : " remote-dependencies )
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

         '{:flow-gl.dataflow.dependent-dataflow/remote-dependencies {:dependent {:remote-2 (:target-cell-1), :remote-1 (:target-cell-1 :target-cell-2)}, :dependent-2 {:remote-2 (:target-cell-1)}}, :flow-gl.dataflow.dependent-dataflow/remote-dataflows-with-changed-dependencies #{:remote-2 :remote-1 :remeote-3}})))

(defn snapshot-remote-dataflows [dataflow]
  (assoc dataflow
    ::remote-dataflows (utils/map-vals (::remote-dataflow-atoms dataflow)
                                 deref)))

(defn get-remote-value [dataflow remote-dataflow cell]
  (logged-access/add-read {::type ::remote-dependency
                           ::remote-dataflow remote-dataflow
                           ::cell cell})

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
        dependent-dataflow-atom (atom (create {}))]

    (add-remote-dataflow dependent-dataflow-atom :remote-1 dependable-dataflow-atom)

    (try
      (swap! dependable-dataflow-atom dataflow/define :foo 1)
      (swap! dependent-dataflow-atom snapshot-remote-dataflows)

      (swap! dependent-dataflow-atom dataflow/define :dependent-foo (fn [dataflow]
                                                                      (+ 1
                                                                         (get-remote-value dataflow :remote-1 :foo))))

                                        ;(flow-gl.debug/debug :dataflow "dependnet-dataflow-now : " (apply hash-map @dependent-dataflow-atom))
      (flow-gl.debug/debug :dataflow "::remote-dataflows-with-changed-dependencies " (::remote-dataflows-with-changed-dependencies @dependent-dataflow-atom))

      (refresh-remote-dataflow-dependencies dependent-dataflow-atom )

      (is (= (dataflow/unlogged-get-value @dependent-dataflow-atom :dependent-foo)
             2))

      (swap! dependable-dataflow-atom dataflow/define :foo 2)
      (dependable-dataflow/notify-dependents dependable-dataflow-atom)

      (Thread/sleep 500)

      (swap! dependent-dataflow-atom snapshot-remote-dataflows)
      (swap! dependent-dataflow-atom dataflow/propagate-changes)

      (is (= (dataflow/unlogged-get-value @dependent-dataflow-atom :dependent-foo)
             3))

      (finally
        (dispose @dependent-dataflow-atom)))))


(debug/reset-log)

(comment
  (debug/set-active-channels :dataflow))

(run-tests)

(debug/write-log)

(comment
  (db :text "foo")
  (editor :text [:db :text])
  )

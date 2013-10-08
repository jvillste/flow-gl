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

(defn create []
  {::remote-dataflow-atoms {}
   ::notification-channels {}
   ::remote-dependencies {}
   ::new-remote-dependencies {}
   ::remote-dataflows-to-be-refreshed #{}})


(defn add-remote-dataflow [dataflow-atom remote-dataflow-key remote-dataflow-atom]
  (swap! dataflow-atom (fn [dataflow]
                         (let [notification-channel (async/chan 10)]
                           (-> dataflow
                               (assoc-in [::remote-dataflow-atoms remote-dataflow-key] remote-dataflow-atom)
                               (assoc-in [::notification-channels remote-dataflow-key] notification-channel))))))


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
           (remote-dataflow-dependencies dataflow remote-dataflow))))

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
        (update-in [::remote-dataflows-to-be-refreshed] into (keys remote-dependencies))
        (base-set-dependencies dependent dependencies))))

(deftest set-dependencies-test
  (is (= (set-dependencies (fn [dataflow _ _] dataflow)

                           {::remote-dataflows-to-be-refreshed #{:remeote-3}
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

(defn update-cell [base-update-cell dataflow cell]
  )

(defn undefine [base-undefine dataflow cell])


(defn start [dataflow-atom]
  (async/go (loop [definition (async/<! (::definition-channel @dataflow-atom))]
              (when definition
                (let [[cell function] definition]
                  (swap! dataflow-atom dataflow/define cell function)
                  (recur (async/<! (::definition-channel @dataflow-atom))))))))

(run-tests)

(comment
  (db :text "foo")
  (editor :text [:db :text])
  )

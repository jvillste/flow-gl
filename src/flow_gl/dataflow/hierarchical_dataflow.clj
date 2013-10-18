(ns flow-gl.dataflow.hierarchical-dataflow
  (:require [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            [flow-gl.multimap :as multimap]
            clojure.set
            [slingshot.slingshot :as slingshot]
            [clojure.data.priority-map :as priority-map]
            [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.dataflow :as dataflow])
  (:use clojure.test
        flow-gl.threading))


(def ^:dynamic current-cell)

(def ^:dynamic child-definitions)

(defn undefine [base-undefine hierarchical-dataflow cell]
  (-> (reduce (fn [dataflow child]
                (undefine dataflow child))
              hierarchical-dataflow
              (get-in hierarchical-dataflow [::children cell]))
      (update-in [::children] dissoc cell)
      (base-undefine cell)))

(defn child-cell-key [parent-cell child-cell]
  (keyword (str (name parent-cell) "_" (name child-cell))))

(defn update-cell [base-update-cell hierarchical-dataflow cell]
  (let [new-child-definitions (atom [])
        new-dataflow (binding [current-cell cell
                               child-definitions new-child-definitions]
                       (base-update-cell hierarchical-dataflow
                                         cell))
        removed-child-cells (clojure.set/difference (get-in hierarchical-dataflow [::children cell])
                                                    (apply hash-set (map :child-cell @new-child-definitions)))]

    (debug/debug :dataflow "updating " cell)

    (-> (reduce (fn [dataflow {:keys [child-cell function action]}]
                  (-> dataflow
                      (update-in [::children] #(multimap/add % cell child-cell))
                      (as-> dataflow
                            (case action
                              :define (dataflow/define dataflow child-cell function)
                              :initialize (dataflow/initialize dataflow child-cell function)))))
                new-dataflow
                @new-child-definitions)

        (dataflow/undefine-many (if (= (get new-dataflow cell)
                                       :dataflow/undefined)
                                  #{}
                                  removed-child-cells))

        (as-> dataflow
              (reduce (fn [dataflow removed-child-cell]
                        (update-in dataflow [::children] #(multimap/del % cell removed-child-cell)))
                      dataflow
                      removed-child-cells)))))

(defn create-child-definitions [cells-and-functions action]
  (vec (for [[child-cell function] (partition 2 cells-and-functions)]
         {:child-cell (child-cell-key current-cell child-cell)
          :function function
          :action action})))

(defn define-children [& cells-and-functions]
  (swap! child-definitions concat (create-child-definitions cells-and-functions :define)))

(defn initialize-children [& cells-and-functions]
  (swap! child-definitions concat (create-child-definitions cells-and-functions :initialize)))

(defn get-child-value [dataflow child-cell]
  (dataflow/get-value dataflow (child-cell-key current-cell child-cell)))



;; PROTOCOL

(defrecord HierarchicalDataflow [])

(extend HierarchicalDataflow
  dataflow/Dataflow
  (merge base-dataflow/dataflow-implementation
         {:undefine (partial undefine base-dataflow/undefine)
          :update-cell (partial update-cell base-dataflow/update-cell)}))


;; CREATE

(defn create [storage]
  (-> (base-dataflow/create storage)
      (assoc ::children {})
      (map->HierarchicalDataflow)))

;; TESTS

(debug/reset-log)

(comment
(debug/set-active-channels :dataflow))

(deftest hierarchical-dataflow-test
  (let [dataflow (-> (create {})
                     (dataflow/define :foo 3)
                     (dataflow/define :foo2 (fn [dataflow]
                                              (initialize-children :x 1
                                                                   :y 2)
                                              (dotimes [n (dataflow/get-value dataflow :foo)]
                                                (define-children (keyword (str "child-" n)) n))
                                              (+ 1 (get-child-value dataflow :x))))
                     (dataflow/define :foo 2)
                     (dataflow/propagate-changes)
                     (base-dataflow/debug-dataflow))]
    (is (= (dataflow/get-value dataflow :foo2)
           2))

    (is (= (dataflow/get-value dataflow :foo2_x)
           1))

    (is (= (dataflow/is-defined? dataflow :foo2_child-2)
           false))))

(run-tests)
(debug/write-log)

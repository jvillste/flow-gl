(ns flow-gl.hierarchical-dataflow
  (:require [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            clojure.set
            [slingshot.slingshot :as slingshot]
            [clojure.data.priority-map :as priority-map]
            [flow-gl.triple-dataflow :as triple-dataflow]
            [flow-gl.p-dataflow :as dataflow])
  (:use clojure.test
        flow-gl.threading))

;; UTILITIES

(defn multimap-add
  "Adds key-value pairs to the multimap."
  ([mm k v]
     (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs]
     (apply multimap-add (multimap-add mm k v) kvs)))

(defn multimap-del
  "Removes key-value pairs from the multimap."
  ([mm k v]
     (assoc mm k (disj (get mm k) v)))
  ([mm k v & kvs]
     (apply multimap-del (multimap-del mm k v) kvs)))


(def ^:dynamic current-cell)

(def ^:dynamic child-definitions)


(defn undefine [hierarchical-dataflow cell]
  (-> (reduce (fn [dataflow child]
                (undefine hierarchical-dataflow child))
              hierarchical-dataflow
              (get-in hierarchical-dataflow [::children cell]))
      (update-in [::children] dissoc cell)
      (triple-dataflow/undefine cell)))


(defn update-cell [hierarchical-dataflow cell]
  (let [new-child-definitions (atom [])
        new-dataflow (binding [current-cell cell
                               child-definitions new-child-definitions]
                       (triple-dataflow/update-cell hierarchical-dataflow
                                                    cell))
        new-child-cells (for [[cell function] (partition 2 @new-child-definitions)] cell)
        removed-child-cells (clojure.set/difference (get-in hierarchical-dataflow [::children cell])
                                                    new-child-cells)]
    (debug/debug :dataflow "updating " cell)

    (-> (reduce (fn [dataflow [child-cell function]]
                  (-> dataflow
                      (update-in [::children] #(multimap-add % cell child-cell))
                      (dataflow/define child-cell function)))
                new-dataflow
                (partition 2 @new-child-definitions))
        (dataflow/undefine-many (if (= (get new-dataflow cell)
                                       :dataflow/undefined)
                                  #{}
                                  removed-child-cells))
        ((fn [dataflow]
           (if (not (empty? removed-child-cells))
             (reduce (fn [dataflow removed-child-cell]
                       (update-in dataflow [::children] #(multimap-del % cell removed-child-cell)))
                     dataflow
                     removed-child-cells)
             dataflow))))))



(defn define-children [& paths-and-functions]
  (swap! child-definitions concat paths-and-functions))


;; PROTOCOL

(defrecord HierarchicalDataflow [])

(extend HierarchicalDataflow
  dataflow/Dataflow
  (merge triple-dataflow/dataflow-implementation
         {:undefine undefine
          :update-cell update-cell}))


;; CREATE

(defn create [storage]
  (-> (triple-dataflow/create storage)
      (assoc ::children {})
      (map->HierarchicalDataflow)))

;; TESTS

(debug/reset-log)

(comment
  (debug/set-active-channels :dataflow))

(deftest get-value-or-nil-test
  (let [dataflow (-> (create {})
                     (dataflow/define :foo 2)
                     (dataflow/define :foo2 (fn [dataflow]
                                              (define-children :foo2.2 (fn [_] 1))
                                              (+ 1 (dataflow/get-value dataflow :foo))))

                     (dataflow/define :foo2 (fn [dataflow]
                                              (define-children :foo2.1 (fn [_] 1))
                                              (+ 1 (dataflow/get-value dataflow :foo))))
                     (dataflow/define :foo 3)
                     (dataflow/propagate-changes)
                     ((fn [dataflow] (triple-dataflow/debug-dataflow dataflow) dataflow)))]
    (is (= (dataflow/get-value dataflow :foo2)
           4))

    (is (= (dataflow/get-value dataflow :foo2.1)
           1))

    (is (= (dataflow/is-defined? dataflow :foo2.2)
           false))))

(run-tests)
(debug/write-log)

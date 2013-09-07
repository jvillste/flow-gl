(ns flow-gl.hierarchical-dataflow
  (:require [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            clojure.set
            [slingshot.slingshot :as slingshot]
            [clojure.data.priority-map :as priority-map]
            [flow-gl.triple-dataflow :as dataflow])
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


(def ^:dynamic current-cell nil)


(defn create [storage]
  (assoc (dataflow/create storage)
    ::children {}))


(defn undefine [hierarchical-dataflow cell]
  (-> (reduce (fn [dataflow child]
                (undefine hierarchical-dataflow child))
              hierarchical-dataflow
              (get-in hierarchical-dataflow [::children path]))
      (update-in [::children] dissoc cell)
      (dataflow/undefine cell)))



(defn update-value [hierarchical-dataflow cell]
  (let [old-children (get-in hierarchical-dataflow [::children cell])
        new-dataflow (binding [current-cell cell]
                       (dataflow/update-value (assoc-in hierarchical-dataflow [::children cell] #{})
                                              cell))]
    (dataflow/undefine-many new-dataflow
                            (if (= (get new-dataflow cell)
                                   :dataflow/undefined)
                              #{}
                              (clojure.set/difference old-children
                                                      (get-in new-dataflow [::children cell]))))))


(defn define
  ([dataflow cell function & keys-and-functions]
     (apply dataflow/define dataflow cell function keys-and-functions))

  ([hierarchical-dataflow cell function]
     (-> (update-in hierarchical-dataflow [::children] #(multimap-add % current-cell cell))
         (dataflow/define cell function))))


(defn initialize)

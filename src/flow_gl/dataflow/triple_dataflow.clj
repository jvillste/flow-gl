(ns flow-gl.triple-dataflow
  (:require [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.dataflow :as dataflow])
  (:use clojure.test))

(defn create-entity []
  (keyword (str "entity-" (rand-int 1000000))))

(defn set [dataflow subject predicate object]
  (dataflow/define dataflow [subject predicate] object))

(defn get [dataflow subject predicate]
  (dataflow/get-value dataflow [subject predicate]))

(defn properties [dataflow entity]
  (map second (filter #(= (first %)
                          entity)
                      (dataflow/cells dataflow))))

(deftype EntityMapEntry [dataflow entity predicate]
  clojure.lang.IMapEntry
  (getKey [_] predicate)
  (getValue [_] (get dataflow entity predicate)))

(defprotocol EntityProtocol
  (modifications [entity]))

(deftype Entity [dataflow modifications entity-id]
  EntityProtocol
  (modifications [_] modifications)

  clojure.lang.IPersistentMap
  (assoc [entity k v] (Entity. dataflow (assoc modifications k v) entity-id))
  (assocEx [_ k v])
  (without [_ k])

  java.lang.Iterable
  (iterator [this])

  clojure.lang.Associative
  (containsKey [_ k]
    (some #{k} (properties dataflow entity-id)))
  (entryAt [_ k]
    (get dataflow entity-id k))

  clojure.lang.IPersistentCollection
  (count [_])
  (cons [_ o])
  (empty [_])
  (equiv [_ o])

  clojure.lang.Seqable
  (seq [_]
    (for [property (properties dataflow entity-id)]
      (EntityMapEntry. dataflow entity-id property)))

  clojure.lang.ILookup
  (valAt [_ k]
    (get dataflow entity-id k))
  (valAt [_ k not-found]
    (if (dataflow/is-defined? dataflow k)
      (get dataflow entity-id k)
      not-found)))

(defn get-entity [dataflow entity-id]
  (Entity. dataflow {} entity-id))

(deftest properties-test
  (let [dataflow (-> (base-dataflow/create {})
                     (set :entity :property-1 :property-1-value)
                     (set :entity :property-2 :property-2-value))]
    (is (= (properties dataflow :entity)
           '(:property-2 :property-1)))))

(deftest entity-test
  (let [entity-id (create-entity)
        dataflow (-> (base-dataflow/create {})
                     (set entity-id :property-1 :property-1-value)
                     (set entity-id :property-2 :property-2-value))
        entity (get-entity dataflow entity-id)]

    (is (= (:property-1 entity)
           :property-1-value))

    (is (= (keys entity)
           '(:property-2 :property-1)))

    (is (= (modifications (assoc entity :foo 1))
           {:foo 1}))))


(run-tests)

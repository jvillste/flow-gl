(ns flow-gl.triple-dataflow
  (:require [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.dataflow :as dataflow])
  (:use clojure.test))


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

(deftype Entity [dataflow entity]
  clojure.lang.IPersistentMap
  (assoc [_ k v])
  (assocEx [_ k v])
  (without [_ k])

  java.lang.Iterable
  (iterator [this])

  clojure.lang.Associative
  (containsKey [_ k]
    (some #{k} (properties dataflow entity)))
  (entryAt [_ k]
    (get dataflow entity k))

  clojure.lang.IPersistentCollection
  (count [_])
  (cons [_ o])
  (empty [_])
  (equiv [_ o])

  clojure.lang.Seqable
  (seq [_]
    (for [property (properties dataflow entity)]
      (EntityMapEntry. dataflow entity property)))

  clojure.lang.ILookup
  (valAt [_ k]
    (get dataflow entity k))
  (valAt [_ k not-found]
    (if (dataflow/is-defined? dataflow k)
      (get dataflow entity k)
      not-found)))

(deftest properties-test
  (let [dataflow (-> (base-dataflow/create {})
                     (set :entity :property-1 :property-1-value)
                     (set :entity :property-2 :property-2-value))]
    (is (= (properties dataflow :entity)
           '(:property-2 :property-1)))))

(deftest entity-test
  (let [dataflow (-> (base-dataflow/create {})
                     (set :entity :property-1 :property-1-value)
                     (set :entity :property-2 :property-2-value))
        entity (Entity. dataflow :entity)]

    (is (= (:property-1 entity)
           :property-1-value))

    (is (= (keys entity)
           '(:property-2 :property-1)))

    (is (= (assoc entity :foo 1)
           '(:property-2 :property-1)))

    ))


(run-tests)

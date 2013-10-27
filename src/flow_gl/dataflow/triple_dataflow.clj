(ns flow-gl.triple-dataflow
  (:require [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.dataflow :as dataflow])
  (:use clojure.test))

(defn create-entity-id []
  (keyword (str "entity-" (rand-int 1000000))))

(defn create-entity [data]
  (assoc data
    ::entity-id (create-entity-id)))

(defn entity? [data]
  (and  (map? data)
        (contains? data ::entity-id)))

(defn entity-vector? [data]
  (and  (vector? data)
        (every? #(contains? % ::entity-id)
                data)))


(defn create-entity-reference [entity-id]
  {::type :entity-reference
   ::entity-id entity-id})


(defn set [dataflow subject predicate object]
  (dataflow/define dataflow [subject predicate] object))

(defn get [dataflow subject predicate]
  (dataflow/get-value dataflow [subject predicate]))

(defn properties [dataflow entity]
  (map second (filter #(= (first %)
                          entity)
                      (dataflow/cells dataflow))))

(defn save-entity [dataflow entity]
  (reduce (fn [dataflow key]
            (set dataflow (::entity-id entity) key (key entity)))
          dataflow
          (remove #{::entity-id} (keys entity))))


(defn create-references [dataflow value]
  (cond (entity? value)
        [(save-entity dataflow value)
         (create-entity-reference (::entity-id value))]

        (entity-vector? value)
        [(reduce save-entity dataflow value)
         (vec (map create-entity-reference (map ::entity-id value)))]

        :default
        [dataflow
         value]))

(defn assoc-new
  ([map key val] (if (contains? map key)
                   map
                   (assoc map key val)))
  ([map key val & kvs]
     (let [ret (assoc-new map key val)]
       (if kvs
         (if (next kvs)
           (recur ret (first kvs) (second kvs) (nnext kvs))
           (throw (IllegalArgumentException.
                   "assoc-new expects even number of arguments after map/vector, found odd number")))
         ret))))

(deftype EntityMapEntry [dataflow entity predicate]
  clojure.lang.IMapEntry
  (getKey [_] predicate)
  (getValue [_] (get dataflow entity predicate)))

(defprotocol EntityProtocol
  (get-dataflow [entity])
  (get-entity-id [entity]))


(deftype Entity [dataflow entity-id]
  EntityProtocol
  (get-dataflow [entity] dataflow)
  (get-entity-id [entity] entity-id)

  clojure.lang.IPersistentMap
  (assoc [entity key value] (let [[dataflow value] (create-references dataflow value)]
                              (Entity. (set dataflow entity-id key value)
                                       entity-id)))
  (assocEx [_ k v])
  (without [_ k])

  java.lang.Iterable
  (iterator [this])

  clojure.lang.Associative
  (containsKey [entity k]
    (if (some #{k} (keys entity))
      true false))
  (entryAt [_ k]
    (get dataflow entity-id k))

  clojure.lang.IPersistentCollection
  (count [_])
  (cons [_ o])
  (empty [_])
  (equiv [_ o])

  clojure.lang.Seqable
  (seq [_]
    (let [properties (properties dataflow entity-id)]
      (if (empty? properties)
        nil
        (for [property properties]
          (EntityMapEntry. dataflow entity-id property)))))

  clojure.lang.ILookup
  (valAt [_ k]
    (get dataflow entity-id k))
  (valAt [_ k not-found]
    (if (dataflow/is-defined? dataflow k)
      (get dataflow entity-id k)
      not-found)))


(defn define-child [parent-entity key function]
  (let [entity-id (if (contains? parent-entity key)
                    (::entity-id (key parent-entity))
                    (create-entity-id))]

    (println "id:" entity-id)

    (-> (Entity. (get-dataflow parent-entity)
                 entity-id)
        function
        get-dataflow
        (Entity. (get-entity-id parent-entity))
        (assoc key (create-entity-reference entity-id)))))


(deftest properties-test
  (let [dataflow (-> (base-dataflow/create {})
                     (set :entity :property-1 :property-1-value)
                     (set :entity :property-2 :property-2-value))]
    (is (= (properties dataflow :entity)
           '(:property-2 :property-1)))))

(deftest entity-test
  (let [entity-id (create-entity-id)
        dataflow (-> (base-dataflow/create {})
                     (set entity-id :property-1 :property-1-value)
                     (set entity-id :property-2 :property-2-value))
        entity (Entity. dataflow entity-id)]

    (is (= (:property-1 entity)
           :property-1-value))

    (is (= (keys entity)
           '(:property-2 :property-1)))

    (is (= (contains? entity :property-1)
           true))

    (is (= (seq (Entity. dataflow (create-entity-id)))
           nil))

    (is (= (-> (assoc entity :foo 1)
               get-dataflow
               (get entity-id :foo))
           1))


    (let [dataflow (-> entity
                       (assoc :foo {:bar 1
                                    ::entity-id :entity-1}
                              :bar {:x :y}
                              :baz [{::entity-id :entity-2
                                     :bar 2}
                                    {::entity-id :entity-3
                                     :bar 3}]

                              :foobar [{::entity-id :entity-4
                                        :bar {::entity-id :entity-5
                                              :bar 2}}

                                       {::entity-id :entity-6
                                        :bar 3}])
                       get-dataflow)]

      (is (= (get dataflow entity-id :foo)
             {:flow-gl.triple-dataflow/type :entity-reference,
              :flow-gl.triple-dataflow/entity-id :entity-1}))

      (is (= (get dataflow :entity-1 :bar)
             1))

      (is (= (get dataflow entity-id :bar)
             {:x :y}))

      (is (= (get dataflow entity-id :baz)
             [{:flow-gl.triple-dataflow/type :entity-reference
               :flow-gl.triple-dataflow/entity-id :entity-2}

              {:flow-gl.triple-dataflow/type :entity-reference
               :flow-gl.triple-dataflow/entity-id :entity-3}]))

      (is (= (get dataflow entity-id :foobar)
             [{:flow-gl.triple-dataflow/type :entity-reference, :flow-gl.triple-dataflow/entity-id :entity-4}
              {:flow-gl.triple-dataflow/type :entity-reference, :flow-gl.triple-dataflow/entity-id :entity-6}]))

      #_(is (= (get dataflow :entity-4 :bar)
               {:flow-gl.triple-dataflow/type :entity-reference, :flow-gl.triple-dataflow/entity-id :entity-5})))


    (let [dataflow (-> entity
                       (define-child :child-view (fn [state]
                                                   (let [state (assoc-new state :contents "foobar")]
                                                     (assoc state :view {:text (:contents state)}))))
                       get-dataflow)]

      (is (= (-> (get dataflow entity-id :child-view)
                 ::entity-id)
             {:text "foobar"})))))


(run-tests)

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

(defn get-entity [dataflow subject predicate]
  (Entity. dataflow (::entity-id (dataflow/get-value dataflow [subject predicate]))))

(defn get-value-or-entity [dataflow subject predicate]
  (let [value (get dataflow subject predicate)]
    (if (and (map? value)
             (contains? value ::entity-id))
      (Entity. dataflow (::entity-id value))
      value)))

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
    (get-value-or-entity dataflow entity-id k))
  (valAt [_ k not-found]
    (if (dataflow/is-defined? dataflow k)
      (get-value-or-entity dataflow entity-id k)
      not-found)))


(defn initialize-new-entity [parent-entity key function]
  (let [entity-id (if (contains? parent-entity key)
                    (::entity-id (key parent-entity))
                    (create-entity-id))]

    (-> (Entity. (get-dataflow parent-entity)
                 entity-id)
        function
        get-dataflow
        (Entity. (get-entity-id parent-entity))
        (assoc key (create-entity-reference entity-id)))))

(defn assoc-with-this [entity key function]
  (assoc entity key (fn [dataflow]
                      (let [state (Entity. dataflow (get-entity-id entity))]
                        (function state)))))

(def ^:dynamic delayed-applications)

(defn with-delayed-applications [state function]
  (binding [delayed-applications (atom [])]
    (let [state (function state)]
      (reduce (fn [state function]
                (function state))
              state
              @delayed-applications))))

(defn apply-delayed [function]
  (swap! delayed-applications conj function))

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
                       (initialize-new-entity :child-view (fn [state]
                                                            (-> state
                                                                (assoc-new :contents "foobar")
                                                                (assoc-with-this :view (fn [state]
                                                                                         {:text (str "bar =" (:contents state))})))))
                       get-dataflow)
          dataflow-after-content-change (-> (get-entity dataflow entity-id :child-view)
                                            (assoc :contents "foobar 2")
                                            get-dataflow
                                            dataflow/propagate-changes)
          ]

      (is (= (:view (get-entity dataflow entity-id :child-view))
             {:text "bar =foobar"}))

      (is (= (:view (get-entity dataflow-after-content-change entity-id :child-view))
             {:text "bar =foobar 2"})))


    (let [child-view (fn [text state]
                       (assoc state :view
                              {:child-text text}))
          init-and-call (fn [key view]
                          (do (apply-delayed (fn [state]
                                               (initialize-new-entity state key view)))
                              {:call key}))

          root-view (fn [state]
                      (with-delayed-applications state
                        (fn [state]
                          (assoc state :view
                                 [(init-and-call :child-view-1 (partial child-view "text-1"))
                                  (init-and-call :child-view-2 (partial child-view "text-2"))]))))

          state (initialize-new-entity entity
                                       :root-view root-view)]

      (is (= (:view (:root-view state))
             [{:call :child-view-1} {:call :child-view-2}]))

      (is (= (-> (:root-view state)
                 :child-view-1
                 :view)
             {:child-text "text-1"})))))





(run-tests)

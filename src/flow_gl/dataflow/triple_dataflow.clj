(ns flow-gl.triple-dataflow
  (:require [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.dataflow :as dataflow])
  (:use clojure.test))

(defn create-entity-id []
  (keyword (str "entity-" (rand-int 1000000))))

(defn create-entity [data]
  (assoc data
    ::entity-id (create-entity-id)))

(defn new-entity? [value]
  (and (map? value)
       (= (::type value)
          :new-entity)))

(defn new-entity-vector? [data]
  (and (vector? data)
       (every? new-entity? data)))

(defn entity-reference? [data]
  (and  (map? data)
        (= (::type data)
           :entity-reference)))

(defn entity-reference-vector? [data]
  (and (vector? data)
       (every? entity-reference? data)))


(defn set [dataflow subject predicate object]
  (dataflow/define dataflow [subject predicate] object))

(defn get [dataflow subject predicate]
  (dataflow/get-value dataflow [subject predicate]))

(defn undefine [dataflow subject predicate]
  (dataflow/undefine dataflow [subject predicate]))

(defn properties [dataflow entity-id]
  (map second (filter #(= (first %)
                          entity-id)
                      (dataflow/cells dataflow))))

(defn save-entity [dataflow entity]
  (reduce (fn [dataflow key]
            (set dataflow (::entity-id entity) key (key entity)))
          dataflow
          (remove #{::entity-id ::type} (keys entity))))




(def create-entity-reference-for-id)

(defn create-references [dataflow value]
  (cond

   (entity? value)
   [(get-dataflow value)
    (create-entity-reference value)]

   (new-entity? value)
   [(save-entity dataflow value)
    (create-entity-reference-for-id (::entity-id value))]

   (new-entity-vector? value)
   [(reduce save-entity dataflow value)
    (vec (map create-entity-reference-for-id (map ::entity-id value)))]

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
  (get-entity-id [entity])
  (get-definitions [entity])
  (add-definition [entity key])
  (reset-definitions [entity]))

(def get-value-or-entity)

(deftype Entity [dataflow entity-id definitions]
  EntityProtocol
  (get-dataflow [entity] dataflow)
  (get-entity-id [entity] entity-id)
  (get-definitions [entity] definitions)
  (add-definition [entity key] (Entity. dataflow entity-id (conj definitions key)))
  (reset-definitions [entity] (Entity. dataflow entity-id #{}))

  clojure.lang.IPersistentMap
  (assoc [entity key value] (let [[dataflow value] (create-references dataflow value)]
                              (Entity. (set dataflow entity-id key value)
                                       entity-id
                                       (conj definitions key))))
  (assocEx [_ k v])
  (without [_ key] (Entity. (undefine dataflow entity-id key) entity-id definitions))

  java.lang.Iterable
  (iterator [this])

  clojure.lang.Associative
  (containsKey [entity k]
    (if (some #{k} (keys entity))
      true false))
  (entryAt [_ k]
    (get-value-or-entity dataflow entity-id k))

  clojure.lang.IPersistentCollection
  (count [_])

  (cons [_ o])
  (empty [_])
  (equiv [this that]
    (and (entity? that)
         (= (keys this)
            (keys that))
         (every? #(= (% this)
                     (% that))
                 (keys this))))

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

(defn create-entity-reference-for-id [entity-id]
  {::type :entity-reference
   ::entity-id entity-id})

(defn create-entity-reference [entity]
  (create-entity-reference-for-id (get-entity-id entity)))



(defn create-entity
  ([dataflow entity-id]
     (Entity. dataflow entity-id #{}))

  ([dataflow]
     (Entity. dataflow (create-entity-id) #{})))

(defn get-entity [dataflow subject predicate]
  (create-entity dataflow (::entity-id (dataflow/get-value dataflow [subject predicate]))))

(defn other-entity [entity entity-id]
  (create-entity (get-dataflow entity) entity-id))

(defn get-value-or-entity [dataflow subject predicate]
  (let [value (get dataflow subject predicate)]
    (cond
     (entity-reference? value)
     (create-entity dataflow (::entity-id value))

     (entity-reference-vector? value)
     (vec (map #(create-entity dataflow (::entity-id %))
               value))

     :default
     value)))

(defn initialize-new-entity [parent-entity key function]
  (let [new-entity (if (contains? parent-entity key)
                     (key parent-entity)
                     (create-entity (get-dataflow parent-entity)
                                    (create-entity-id)))]
    (-> new-entity
        function
        (other-entity (get-entity-id parent-entity))
        (assoc key (create-entity-reference new-entity)))))


(defn entity? [value]
  (= (type value)
     Entity))

(deftest entity?-test
  (is (= (entity? (create-entity {} :id))
         true))

  (is (= (entity? {})
         false)))

(def undefine-entity)

(defn undefine-keys [entity keys]
  (reduce (fn [entity key-to-be-undefined]
            (let [value (key-to-be-undefined entity)]
              (if (entity? value)
                (dissoc (create-entity (undefine-entity (get-dataflow entity)
                                                        (get-entity-id value))
                                       (get-entity-id entity))
                        key-to-be-undefined)
                (dissoc entity key-to-be-undefined))))
          entity
          keys))

(defn undefine-entity [dataflow entity-id]
  (undefine-keys (create-entity dataflow entity-id) (properties dataflow entity-id)))

(defn keys-to-be-undefined [entity]
  (let [definitions (get-definitions entity)]
    (filter #(not (definitions %))
            (keys entity))))

(deftest keys-to-be-undefined-test
  (is (= (-> (base-dataflow/create {})
             (create-entity :entity-1)
             (assoc :foo 1
                    :bar 2)
             (reset-definitions)
             (assoc :foo 1)
             (keys-to-be-undefined))
         '(:bar))))

(defn collect-garbage [entity]
  (undefine-keys entity (keys-to-be-undefined entity)))

(deftest collect-garbage-test
  (is (= (-> (base-dataflow/create {})
             (create-entity :entity-1)
             (assoc :foo 1
                    :bar 2)
             (reset-definitions)
             (assoc :foo 1)
             (collect-garbage)
             (keys))
         '(:foo))))

(defn assoc-with-this [entity key function]
  (assoc entity key (fn [dataflow]
                      (let [state (create-entity dataflow (get-entity-id entity))]
                        (function state)))))

(def ^:dynamic delayed-applications)

(defn with-delayed-applications-function [state function]
  (binding [delayed-applications (atom [])]
    (let [state (function state)]
      (reduce (fn [state function]
                (function state))
              state
              @delayed-applications))))

(defmacro with-delayed-applications [value & body]
  `(with-delayed-applications-function ~value (fn [~value] ~@body)))


(defn apply-delayed [function]
  (swap! delayed-applications conj function))

(deftest properties-test
  #_(let [dataflow (-> (base-dataflow/create {})
                       (set :entity :property-1 :property-1-value)
                       (set :entity :property-2 :property-2-value))]
      (is (= (properties dataflow :entity)
             '(:property-2 :property-1)))))

(deftest initialize-new-entity-test
  #_(let [entity-id (create-entity-id)
          entity (create-entity (base-dataflow/create {}) entity-id)
          dataflow (-> entity
                       (initialize-new-entity :child-view (fn [state]
                                                            (-> state
                                                                (assoc-new :contents "foobar")
                                                                (assoc-with-this :view (fn [state]
                                                                                         {:text (str "bar =" (:contents state))})))))
                       get-dataflow)
          dataflow-after-content-change (-> (get-entity dataflow entity-id :child-view)
                                            (assoc :contents "foobar 2")
                                            get-dataflow
                                            dataflow/propagate-changes)]

      (is (= (:view (get-entity dataflow entity-id :child-view))
             {:text "bar =foobar"}))

      (is (= (:view (get-entity dataflow-after-content-change entity-id :child-view))
             {:text "bar =foobar 2"}))))

(defn new-entity [& key-values]
  (merge {::type :new-entity
          ::entity-id (create-entity-id)}
         (apply hash-map key-values )))

(deftest entity-equiv-test
  (let [dataflow (base-dataflow/create {})]
    (is (= (-> (create-entity dataflow)
               (assoc :foo 1))
           (-> (create-entity dataflow)
               (assoc :foo 1))))

    (is (not= (-> (create-entity dataflow)
                  (assoc :foo 3))
              (-> (create-entity dataflow)
                  (assoc :foo 1)))

        (not= (-> (create-entity dataflow)
                  (assoc :foo 1)
                  (assoc :bar 1))
              (-> (create-entity dataflow)
                  (assoc :foo 1))))))

(deftest assoc-new-entity-test
  (let [entity-id (create-entity-id)
        entity (-> (create-entity (base-dataflow/create {}) entity-id)
                   (assoc :single-entity (new-entity :bar 1)
                          :map {:bar 2}
                          :entity-vector [(new-entity :bar 3)
                                          (new-entity :bar 4)]

                          :deep-entity-vector [(new-entity :bar (new-entity :bar 5))
                                               (new-entity :bar 6)]

                          :deep-entity (new-entity :bar (new-entity :bar 5))

                          :number 1
                          :string "foo"))
        dataflow (get-dataflow entity)]

    (is (= (get dataflow (get-entity-id (:single-entity entity)) :bar)
           1))

    (is (= (:bar (:single-entity entity))
           1))

    (is (= (:bar (:map entity))
           2))

    (is (= (:bar (first (:entity-vector entity)))
           3))

    (is (= (:bar (:bar (first (:deep-entity-vector entity))))
           5))

    (is (= (-> (update-in entity [:deep-entity :bar :bar] inc)
               (get-in [:deep-entity :bar :bar]))
           6))

    (is (= (:number entity)
           1))

    (is (= (:string entity)
           "foo"))))


(deftest entity-test
  (let [entity-id (create-entity-id)
        dataflow (-> (base-dataflow/create {})
                     (set entity-id :property-1 :property-1-value)
                     (set entity-id :property-2 :property-2-value))
        entity (create-entity dataflow entity-id)]

    (is (= (:property-1 entity)
           :property-1-value))

    (is (= (keys entity)
           '(:property-2 :property-1)))

    (is (= (contains? entity :property-1)
           true))

    (is (= (seq (create-entity dataflow (create-entity-id)))
           nil))

    (is (= (-> (assoc entity :foo 1)
               get-dataflow
               (get entity-id :foo))
           1))))

(deftest view-test
  (let [application-state (-> (create-entity (base-dataflow/create {}) :application-state)
                              (assoc :todos [(new-entity :text "do this")
                                             (new-entity :text "do that")] ))

        child-view (fn [todo-id state]
                     (assoc state :view
                            {:child-text (:text (other-entity state todo-id))}))

        init-and-call (fn [key view]
                        (do (apply-delayed (fn [state]
                                             (initialize-new-entity state key view)))
                            {:call key}))

        root-view (fn [state]
                    (with-delayed-applications state
                      (assoc state :view
                             (for [todo (:todos (other-entity state :application-state))]
                               (init-and-call (keyword (str "child-view-" (name (get-entity-id todo))))
                                              (partial child-view (get-entity-id todo)))))))

        state (initialize-new-entity application-state
                                     :root-view root-view)]


    (is (= (let [child-view (:call (first (:view (:root-view state))))]
             (get (get-dataflow state) child-view :view)) 
           nil))))


(run-tests)

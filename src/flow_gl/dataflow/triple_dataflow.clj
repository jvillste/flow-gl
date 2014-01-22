(ns flow-gl.dataflow.triple-dataflow
  (:require [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.dataflow :as dataflow])
  (:use midje.sweet
        clojure.test
        flow-gl.utils))

(def create-entity-reference-for-id)
(def create-entity-reference)
(def entity?)

(defn cell-key [subject predicate]
  [subject predicate])

(defn changes [dataflow]
  (dataflow/changes dataflow))

(defn dependents [dataflow subject predicate]
  (dataflow/dependents dataflow (cell-key subject predicate)))

(defn is-defined? [dataflow subject predicate]
  (dataflow/is-defined? dataflow (cell-key subject predicate)))

(defn create-entity-id []
  (keyword (str "entity-" (rand-int 1000000))))

(defn create-entity [data]
  (assoc data
    ::entity-id (create-entity-id)))

(defn property [subject predicate]
  {:subject subject
   :predicate predicate})

(defn new-entity? [value]
  (and (map? value)
       (= (::type value)
          :new-entity)))

(defn new-entity-vector? [data]
  (and (vector? data)
       (every? new-entity? data)))

(defn entity-reference? [data]
  (and  (map? data)
        (not (entity? data))
        (= (::type data)
           :entity-reference)))

(defn entity-reference-vector? [data]
  (and (vector? data)
       (every? entity-reference? data)))

(def ^:dynamic current-subject)

(defn set-value [dataflow subject predicate object]
  (binding [current-subject subject]
    (dataflow/define dataflow (cell-key subject predicate) object)))

(defn get-value [dataflow subject predicate]
  (dataflow/get-value dataflow (cell-key subject predicate)))

(defn get-property [dataflow property]
  (get-value dataflow (:subject property) (:predicate property)))

(defn undefine [dataflow subject predicate]
  (dataflow/undefine dataflow (cell-key subject predicate)))

(defn properties [dataflow entity-id]
  (map second (filter #(= (first %)
                          entity-id)
                      (dataflow/cells dataflow))))

(defn save-entity [dataflow entity]
  (reduce (fn [dataflow key]
            (set-value dataflow (::entity-id entity) key (key entity)))
          dataflow
          (remove #{::entity-id ::type} (keys entity))))

(defn create-references [dataflow value]
  (cond

   (entity-reference? value)
   [dataflow
    value ]

   (entity? value)
   [(::dataflow value)
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
  (getValue [_] (get-value dataflow entity predicate)))

(defn reset-definitions [entity]
  (assoc entity ::definitions #{}))

(def get-value-or-entity)

(deftype Entity [dataflow entity-id definitions]

  clojure.lang.IPersistentMap
  (assoc [entity key value] (case key
                              ::entity-id (Entity. dataflow
                                                   value
                                                   definitions)
                              ::dataflow (Entity. value
                                                  entity-id
                                                  definitions)
                              ::definitions (Entity. dataflow
                                                     entity-id
                                                     value)
                              (let [[dataflow value] (create-references dataflow value)]
                                (Entity. (set-value dataflow entity-id key value)
                                         entity-id
                                         (conj definitions key)))))

  (without [entity key] (update-in entity [::dataflow] undefine entity-id key))

  clojure.lang.Associative
  (containsKey [entity k]
    (if (some #{k} (keys entity))
      true false))

  clojure.lang.IPersistentCollection
  (equiv [this that]
    (and (map? that)
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
    (case k
      ::entity-id entity-id
      ::dataflow dataflow
      ::definitions definitions
      (get-value-or-entity dataflow entity-id k))))

(defn create-entity-reference-for-id [entity-id]
  {::type :entity-reference
   ::entity-id entity-id})

(defn create-entity-reference [entity]
  (create-entity-reference-for-id (::entity-id entity)))

(defn create-entity
  ([dataflow entity-id]
     (Entity. dataflow entity-id #{}))

  ([dataflow]
     (Entity. dataflow (create-entity-id) #{})))

(defn get-entity [dataflow subject predicate]
  (create-entity dataflow (::entity-id (dataflow/get-value dataflow (cell-key subject predicate)))))

(defn switch-entity [entity entity-or-entity-id]
  (if (entity? entity-or-entity-id)
    (create-entity (::dataflow entity) (::entity-id entity-or-entity-id))
    (create-entity (::dataflow entity) entity-or-entity-id)))

(defn get-value-or-entity [dataflow subject predicate]
  (let [value (get-value dataflow subject predicate)]
    (cond
     (entity-reference? value)
     (create-entity dataflow (::entity-id value))

     (entity-reference-vector? value)
     (vec (map #(create-entity dataflow (::entity-id %))
               value))

     :default
     value)))

(defn initialize-new-entity [parent-entity key function]
  (if (contains? parent-entity key)
    parent-entity
    (let [new-entity (create-entity (::dataflow parent-entity)
                                    (create-entity-id))]
      (-> new-entity
          function
          (switch-entity parent-entity)
          (assoc key (create-entity-reference new-entity))))))

(defn entity? [value]
  (= (type value)
     Entity))

(fact (-> (create-entity (base-dataflow/create))
          (assoc ::entity-id :foo)
          (assoc :bar 1)
          (::dataflow)
          (get-value :foo :bar))
      => 1)

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
                (dissoc (create-entity (undefine-entity (::dataflow entity)
                                                        (::entity-id value))
                                       (::entity-id entity))
                        key-to-be-undefined)
                (dissoc entity key-to-be-undefined))))
          entity
          keys))

(defn undefine-entity [dataflow entity-id]
  (undefine-keys (create-entity dataflow entity-id) (properties dataflow entity-id)))

(defn keys-to-be-undefined [entity]
  (let [definitions (::definitions entity)]
    (filter #(not (definitions %))
            (keys entity))))

(deftest keys-to-be-undefined-test
  (is (= (-> (base-dataflow/create)
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
  (is (= (-> (base-dataflow/create)
             (create-entity :entity-1)
             (assoc :foo 1
                    :bar 2)
             (reset-definitions)
             (assoc :foo 1)
             (collect-garbage)
             (keys))
         '(:foo))))

(defn assoc-with-this
  ([entity key function & keys-and-functions]
     (apply assoc-with-this
            (assoc-with-this entity key function)
            keys-and-functions))

  ([entity key function]
     (assoc entity key (fn [dataflow]
                         (let [state (create-entity dataflow (::entity-id entity))]
                           (function state))))))

(fact setget-test
      (-> (base-dataflow/create)
          (set-value :foo :foobar (fn [dataflow]
                                    (+ (get-value dataflow :foo :bar)
                                       1)))
          (set-value :foo :bar 1)
          dataflow/propagate-changes
          (get-value :foo :foobar))
      => 2)

(deftest assoc-with-this-test
  (is (= (-> (base-dataflow/create)
             (create-entity)
             (assoc :foo 1)
             (assoc-with-this :bar #(+ (:foo %)
                                       1)

                              :foobar #(+ (:bar %)
                                          1))
             (:foobar))
         3)))

(deftest properties-test
  (let [dataflow (-> (base-dataflow/create)
                     (set-value :entity :property-1 :property-1-value)
                     (set-value :entity :property-2 :property-2-value))]
    (is (= (properties dataflow :entity)
           '(:property-2 :property-1)))))

(deftest initialize-new-entity-test
  (let [entity-id (create-entity-id)
        entity (create-entity (base-dataflow/create) entity-id)
        dataflow (-> entity
                     (initialize-new-entity :child-view (fn [state]
                                                          (-> state
                                                              (assoc-new :contents "foobar")
                                                              (assoc-with-this :view (fn [state]
                                                                                       {:text (str "bar =" (:contents state))})))))
                     ::dataflow)
        dataflow-after-content-change (-> (get-entity dataflow entity-id :child-view)
                                          (assoc :contents "foobar 2")
                                          ::dataflow
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
  (let [dataflow (base-dataflow/create)]
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
                  (assoc :foo 1))))

    (is (= (-> (create-entity dataflow)
               (assoc :foo 3))
           {:foo 3}))))

(deftest assoc-new-entity-test
  (let [entity-id (create-entity-id)
        entity (-> (create-entity (base-dataflow/create) entity-id)
                   (assoc :single-entity (new-entity :bar 1)
                          :map {:bar 2}
                          :entity-vector [(new-entity :bar 3)
                                          (new-entity :bar 4)]

                          :deep-entity-vector [(new-entity :bar (new-entity :bar 5))
                                               (new-entity :bar 6)]

                          :deep-entity (new-entity :bar (new-entity :bar 5))

                          :number 1
                          :string "foo"))
        dataflow (::dataflow entity)]

    (is (= (get-value dataflow (::entity-id (:single-entity entity)) :bar)
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

    (is (= (-> (assoc-in entity [:deep-entity :bar :foo] :foobar)
               (get-in [:deep-entity :bar :foo]))
           :foobar))

    (is (= (:number entity)
           1))

    (is (= (:string entity)
           "foo"))))

(deftest entity-test
  (let [entity-id (create-entity-id)
        dataflow (-> (base-dataflow/create)
                     (set-value entity-id :property-1 :property-1-value)
                     (set-value entity-id :property-2 :property-2-value))
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
               ::dataflow
               (get-value entity-id :foo))
           1))))

(deftest dependency-test
  (debug/reset-log)

  (-> (base-dataflow/create)

      (create-entity :root)
      (assoc :foo :root-foo)
      (initialize-new-entity :child (fn [entity]
                                      (assoc entity :foo (fn [dataflow]
                                                           (get-value dataflow :root :foo)))))
      (::dataflow)
      (base-dataflow/debug-dataflow)))


(defn preferred-width [layoutable]
  (case (:type layoutable)
    :child-view-call (get-value base-dataflow/current-dataflow (:child-view-id layoutable) :preferred-width)
    :text (count (:text layoutable))
    :margin (+ 10 (preferred-width (:child layoutable)))
    0))

(defn layout [layoutable requested-width global-x]
  (case (:type layoutable)
    :margin (-> layoutable
                (assoc :global-x global-x)
                (update-in [:child] layout requested-width (+ global-x 10)))
    :child-view-call (do (base-dataflow/apply-to-dataflow (fn [dataflow]
                                                            (-> dataflow
                                                                (set-value (:child-view-id layoutable) :requested-width requested-width)
                                                                (set-value (:child-view-id layoutable) :global-x global-x))))
                         layoutable)
    (assoc layoutable
      :global-x global-x)))


(defmacro view [parameters view-expression]
  (let [state-symbol (first parameters)]
    `(fn ~parameters
       (assoc-with-this ~state-symbol
                        :view (fn [~state-symbol]
                                ~view-expression)

                        :layout (fn [state#]
                                  (layout (:view state#)
                                          (:requested-width state#)
                                          (:global-x state#)))

                        :preferred-width (fn [state#]
                                           (preferred-width (:view state#)))))))

(defn defview [name parameters view-expression]
  '(def ~name (view parameters view-expression)))

(defn child-view-key [& identifiers]
  (keyword (str "child-view-" identifiers)))

(defn call-child-view [parent-view identifiers view & parameters]
  (let [key (apply child-view-key identifiers)
        child-view-id (if (contains? parent-view key)
                        (::entity-id (key parent-view))
                        (create-entity-id))]

    (do (when (not (contains? parent-view key))
          (base-dataflow/apply-to-dataflow (fn [dataflow]
                                             (-> (apply view
                                                        (create-entity dataflow child-view-id)
                                                        parameters)
                                                 (switch-entity  parent-view)
                                                 (assoc key (create-entity-reference-for-id child-view-id))
                                                 ::dataflow))))
        {:type :child-view-call
         :kwey key
         :child-view-id child-view-id})))


(facts layout-test
       (let [child-view (view [state]
                              {:type :margin
                               :child {:type :text
                                       :text "Foo"}})

             root-view (view [state]
                             {:type :margin
                              :child (call-child-view state [:child] child-view)})

             dataflow (-> (create-entity (base-dataflow/create) :application-state)
                          (initialize-new-entity :root-view root-view)
                          :root-view
                          (assoc :requested-width 100
                                 :global-x 0)
                          ::dataflow
                          (dataflow/propagate-changes))
             application-state (create-entity dataflow :application-state)]

         (fact (-> application-state
                   :root-view
                   :preferred-width)
               => 23)

         (fact (-> application-state
                   :root-view
                   ((child-view-key :child))
                   :preferred-width)
               => 13)

         (fact (-> application-state
                   :root-view
                   ((child-view-key :child))
                   :global-x)
               => 10)

         (fact (-> application-state
                   :root-view
                   ((child-view-key :child))
                   :layout
                   :child
                   :global-x)
               => 20)

         (let [application-state (-> application-state
                                     :root-view
                                     ((child-view-key :child))
                                     (assoc-in [:view :child :text] "foobar")
                                     (update-in [::dataflow] dataflow/propagate-changes)
                                     (switch-entity :application-state))]
           (fact (-> application-state
                     :root-view
                     ((child-view-key :child))
                     :preferred-width)
                 => 16)

           #_(doseq [line (base-dataflow/describe-dataflow (::dataflow application-state))]
               (println line)))


         #_(doseq [line (base-dataflow/describe-dataflow dataflow)]
             (println line))))

(facts layout-tests
       (fact text-layout (layout {:type :text} 10 10) => {:type :text :global-x 10})

       (let [view-entity (-> (create-entity (base-dataflow/create) :view-1)
                             (assoc :view {:type :text
                                           :text "foo"}))
             dataflow (set-value (::dataflow view-entity) :view-1 :layout
                                 (fn [state]
                                   (layout {:type :child-view-call
                                            :child-view-id :view-1}
                                           10 10)))]

         (fact view (get-value dataflow :view-1 :view) => {:type :text, :text "foo"})
         (fact layout (get-value dataflow :view-1 :layout) => {:type :child-view-call
                                                               :child-view-id :view-1})))

#_(facts view-definition-test
         (debug/reset-log)
         (let [application-state (-> (create-entity (base-dataflow/create) :application-state)
                                     (assoc :todos [(new-entity :text "do this")
                                                    (new-entity :text "do that")]))

               child-view (view [state property]
                                {:type :text
                                 :text (get-property (::dataflow state) property)})

               root-view (view [state]
                               (forall [todo (:todos (switch-entity state :application-state))]
                                       (call-child-view state
                                                        [(::entity-id todo)]
                                                        child-view
                                                        (property (::entity-id todo) :text))))

               application-state (-> application-state
                                     (initialize-new-entity :root-view root-view)
                                     :root-view
                                     (assoc :requested-width 100
                                            :global-x 0)
                                     (switch-entity :application-state))]

           (doseq [line (base-dataflow/describe-dataflow (dataflow/propagate-changes (::dataflow application-state)))]
             (println line))

           (fact (let [child-view ((:key (first (:view (:root-view application-state)))) (:root-view application-state))]
                   (:view child-view))
                 =>
                 {:type :text, :text "do this"})


           (let [application-state (-> (:todos application-state)
                                       (first)
                                       (assoc :text "foo")
                                       (::dataflow)
                                       (dataflow/propagate-changes)
                                       (create-entity :application-state))]

             (base-dataflow/debug-dataflow (::dataflow application-state))

             (fact (let [child-view ((:key (first (:view (:root-view application-state)))) (:root-view application-state))]
                     (:view child-view))
                   =>
                   {:type :text, :text "foo"})


             (let [child-view ((:key (first (:view (:root-view application-state)))) (:root-view application-state))]
               (:layout child-view))
             =>
             :flow-gl.dataflow.base-dataflow/undefined)))

(comment
  (debug/set-active-channels :all)

  (debug/reset-log)

  (debug/write-log))

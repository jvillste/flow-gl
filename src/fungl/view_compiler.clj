(ns fungl.view-compiler
  (:require [fungl.cache :as cache]
            [clojure.test :refer :all]
            [fungl.callable :as callable]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.set :as set]
            [medley.core :as medley]
            [logga.core :as logga]
            [flow-gl.gui.visuals :as visuals]
            [fungl.swing.root-renderer :as root-renderer]
            [fungl.component :as component]
            [clojure.walk :as walk]
            [fungl.depend :as depend]
            [clojure.string :as string]
            [fungl.util :as util]
            [fungl.id-comparator :as id-comparator]))

(def ^:dynamic state)

(def ^:dynamic id)
(def ^:dynamic applied-view-call-ids)

(defn is-prefix [sequence prefix-candidate]
  (or (= [] prefix-candidate)
      (= (take (count prefix-candidate)
               sequence)
         prefix-candidate)))

(deftest test-is-prefix
  (is (is-prefix [1 2]
                 [1 2]))

  (is (is-prefix [1 2 3]
                 [1 2]))

  (is (is-prefix [1 2 3]
                 []))

  (is (not (is-prefix [1 2 3]
                      [2]))))

(defn sorted-id-set [sequence]
  (apply sorted-set-by
         id-comparator/compare-ids
         sequence))

(defn closest-match [sorted-set value]
  (first (rsubseq sorted-set <= value)))

(deftest test-closest-match
  (is (= [1]
         (closest-match (sorted-id-set [[1]
                                        [1 0]
                                        [2 0]])
                        [1])))

  (is (= [1 0]
         (closest-match (sorted-id-set [[1]
                                        [1 0]
                                        [2 1]])
                        [2 0])))

  (is (= [2 0]
         (closest-match (sorted-id-set [[1]
                                        [1 0]
                                        [2 0]
                                        [2 2]])
                        [2 1])))

  (is (= [2 0]
         (closest-match (sorted-id-set [[1]
                                        [1 0]
                                        [2 0]
                                        [2 2]])
                        [2 0 1]))))

(defn is-prefix-of-some [sorted-set-of-sequences sequence]
  (or (and (= [] sequence)
           (not (empty? sorted-set-of-sequences)))
      (let [closest-match (first (subseq sorted-set-of-sequences
                                         >=
                                         sequence))]
        (if closest-match
          (if (is-prefix closest-match sequence)
            true
            false)
          nil))))

(deftest test-is-prefix-of-some
  (let [sorted-set (sorted-id-set [[1]
                                   [1 0]
                                   [2 0]])]

    (is (is-prefix-of-some sorted-set
                           [1]))

    (is (is-prefix-of-some sorted-set
                           [1 0]))

    (is (is-prefix-of-some sorted-set
                           []))

    (is (is-prefix-of-some (sorted-id-set [[:a :call]])
                           [:a]))

    (is (not (is-prefix-of-some sorted-set
                                [3 0])))))

(defn topmost-node-ids [node-ids]
  (let [all-sorted-node-ids (sort-by identity
                                     id-comparator/compare-ids
                                     node-ids)]
    (loop [remaining-sorted-node-ids (rest all-sorted-node-ids)
           previous-node-id (first all-sorted-node-ids)
           topmost-node-ids [(first all-sorted-node-ids)]]

      (if (empty? remaining-sorted-node-ids)
        topmost-node-ids
        (let [next-node-id (first remaining-sorted-node-ids)]
          (if (is-prefix next-node-id previous-node-id)
            (recur (rest remaining-sorted-node-ids)
                   next-node-id
                   topmost-node-ids)
            (recur (rest remaining-sorted-node-ids)
                   next-node-id
                   (conj topmost-node-ids
                         next-node-id))))))))

(deftest test-topmost-node-ids
  (is (= [[]]
         (topmost-node-ids [[]
                            [1]
                            [1 2]])))
  (is (= [[1] [2]]
         (topmost-node-ids [[1]
                            [1 2]
                            [2]]))))

(defn- view-call? [value]
  (and (vector? value)
       (or (fn? (first value))
           (var? (first value)))))

(defn- scene-graph? [value]
  (map? value))

(defn- apply-view-call [the-id view-call]
  (binding [id the-id]
    (let [[view-function-or-constructor & arguments] view-call
          scene-graph-or-view-call (cond (apply cache/cached?
                                                view-function-or-constructor
                                                arguments)
                                         (apply cache/call!
                                                view-function-or-constructor
                                                arguments)

                                         (contains? (:constructor-cache @state)
                                                    the-id)
                                         (let [view-function (get (:constructor-cache @state)
                                                                  the-id)]
                                           (apply cache/call! view-function arguments))

                                         :else
                                         (let [{:keys [result dependencies]} (cache/call-and-return-result-and-dependencies view-function-or-constructor arguments)]
                                           (if (fn? result)
                                             (do (swap! state
                                                        update
                                                        :constructor-cache
                                                        assoc
                                                        the-id
                                                        result)
                                                 (apply cache/call! result arguments))
                                             (do (cache/put! (cache/function-call-key view-function-or-constructor
                                                                                      arguments)
                                                             result)
                                                 (cache/set-dependencies! view-function-or-constructor
                                                                          arguments
                                                                          dependencies)

                                                 result))))]

      (cond (view-call? scene-graph-or-view-call)
            scene-graph-or-view-call

            (scene-graph? scene-graph-or-view-call)
            (-> scene-graph-or-view-call
                (assoc :id the-id))

            :else
            (throw (Exception. (str "View function did not return a hash map: " view-function-or-constructor)))))))

(defn- apply-metadata [metadata compiled-node]
  (if metadata
    (reduce (fn [node key]
              (if-let [metadata-value (get metadata key)]
                (assoc node key (if (fn? metadata-value)
                                  (metadata-value (get node key))
                                  metadata-value))
                node))
            compiled-node
            (keys metadata))
    compiled-node))

(defn function-class-name-to-function-name [function-class-name]
  (when-let [match (re-matches #".*\$(.*)@.*"
                               function-class-name)]
    (-> match
        (second)
        (string/replace "_" "-"))))

(deftest test-function-class-name-to-function-name
  (is (= "stateful-component"
         (function-class-name-to-function-name "argupedia.ui2$stateful_component@1dfa8582")))
  (is (= nil
         (function-class-name-to-function-name "#'foo.bar/baz"))))

(defn function-name [function]
  (function-class-name-to-function-name (str function)))

(defn compile-node [parent-view-functions id value]
  (assert (bound? #'state)
          "Bindings returned by (state-bindings) should be bound.")

  (assert (not (integer? (:local-id (meta value))))
          "local ids can not be integers")

  (cond (view-call? value)
        (apply-metadata (meta value)
                        (let [id (if (not (empty? parent-view-functions))
                                   (vec (conj id
                                              (or (:local-id (meta value))
                                                  :call)))
                                   id)

                              scene-graph (if-let [scene-graph (get (:scene-graph-cache @state)
                                                                    id)]
                                            (do (prn 'cache-hit!
                                                     (function-name (if (var? (first value))
                                                                      @(first value)
                                                                      (first value)))
                                                     id) ;; TODO: remove me

                                                (swap! applied-view-call-ids
                                                       (fn [applied-view-call-ids]
                                                         (apply conj
                                                                applied-view-call-ids
                                                                (->> (keys (:scene-graph-cache @state))
                                                                     (filter (fn [node-id]
                                                                               (is-prefix node-id
                                                                                          id)))))))
                                                (if (= visuals/render-to-images-render-function
                                                       (:render scene-graph))
                                                  scene-graph
                                                  (assoc scene-graph
                                                         :render visuals/render-to-images-render-function
                                                         :render-on-descend? true)))
                                            (do
                                              (swap! applied-view-call-ids
                                                     conj
                                                     id)

                                              (compile-node (conj parent-view-functions
                                                                  (first value))
                                                            id
                                                            (apply-view-call id value))))

                              dependency-value-map (cache/dependency-value-map (or (get (:constructor-cache @state)
                                                                                        id)
                                                                                   (first value))
                                                                               (rest value))

                              ;; scene-graph (assoc scene-graph
                              ;;                    :view-functions (concat [{:function (first value)
                              ;;                                              :dependencies (for [[dependable _value] dependency-value-map]
                              ;;                                                              (merge {:dependable dependable
                              ;;                                                                      :new-value (depend/current-value dependable)}
                              ;;                                                                     (when-let [old-value (get old-dependency-value-map dependable)]
                              ;;                                                                       {:old-value old-value})))
                              ;;                                              :local-id (:local-id (meta value))}]
                              ;;                                            (:view-functions scene-graph)))
                              ]

                          (when (not (empty? dependency-value-map))
                            (swap! state
                                   update
                                   :node-dependencies
                                   assoc
                                   id
                                   dependency-value-map))

                          (swap! state
                                 update
                                 :scene-graph-cache
                                 assoc
                                 id
                                 scene-graph)

                          (swap! state
                                 update
                                 :view-functions
                                 assoc
                                 id
                                 (first value))

                          scene-graph))

        (:children value)
        (-> value
            (update :children
                    (fn [children]
                      (vec (map-indexed (fn [index child]
                                          (compile-node []
                                                        (vec (conj id
                                                                   (or (:local-id (meta child))
                                                                       (:local-id child)
                                                                       index)))
                                                        child))
                                        children))))
            (assoc :id id))

        :else
        (assoc value
               :id id)))

(defn invalidated-node-ids [node-dependencies]
  (->> node-dependencies
       (filter (fn [[_node-id dependency-value-map]]
                 (some (fn [[dependency value]]
                         (not (= value
                                 (depend/current-value dependency))))
                       dependency-value-map)))
       (map first)))

(defn compile-view-calls [view-call-or-scene-graph]
  (let [sorted-invalidated-node-ids-set (sorted-id-set (invalidated-node-ids (:node-dependencies @state)))
        invalidated-node-ids-set-with-parents (set (filter (fn [node-id]
                                                             (is-prefix-of-some sorted-invalidated-node-ids-set
                                                                                node-id))
                                                           (keys (:scene-graph-cache @state))))]


    (swap! state
           update
           :node-dependencies
           (partial medley/remove-keys
                    invalidated-node-ids-set-with-parents))

    (swap! state
           update
           :scene-graph-cache
           (partial medley/remove-keys
                    invalidated-node-ids-set-with-parents)))

  (binding [applied-view-call-ids (atom #{})]
    (let [result (compile-node []
                               []
                               view-call-or-scene-graph)]

      (swap! state
             update
             :scene-graph-cache
             (partial medley/filter-keys
                      @applied-view-call-ids))

      (swap! state
             update
             :view-functions
             (partial medley/filter-keys
                      @applied-view-call-ids))

      (swap! state
             update
             :node-dependencies
             (partial medley/filter-keys
                      @applied-view-call-ids))

      (swap! state
             update
             :constructor-cache
             (partial medley/filter-keys
                      @applied-view-call-ids))

      result)))

(defn state-bindings []
  {#'state (atom {:constructor-cache {}
                  :node-dependencies {}
                  :scene-graph-cache {}
                  :view-functions {}})})

(deftest test-compile
  (let [test-compile (fn [value]
                       (walk/postwalk (fn [node]
                                        (if (map? node)
                                          (dissoc node
                                                  :render
                                                  :view-functions)
                                          node))
                                      (compile-view-calls value)))]
    (with-bindings (merge (state-bindings)
                          (cache/state-bindings))

      (is (= {:type :root
              :id []}
             (test-compile {:type :root})))

      (let [view (fn []
                   {:type :view})]
        (is (= {:type :view
                :id []}
               (test-compile [view]))))

      (testing "if a view only calls another view, they must have different ids to differentiate their state. This is why there is a :call -keyword inserted into the id"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] [view-2])]
          (is (= {:type :view-2
                  :id [:call]}
                 (test-compile [view-1])))))


      (let [view-2 (fn [] {:type :view-2})
            view-1 (fn [] {:type :view-1
                           :children [[view-2]]})]
        (is (= '{:type :view-1
                 :id []
                 :children ({:type :view-2
                             :id [0]})}
               (test-compile [view-1]))))

      (let [view-3 (fn [] {:type :view-3})
            view-2 (fn [] [view-3])
            view-1 (fn [] {:type :view-1
                           :children [[view-2]]})]
        (is (= '{:type :view-1
                 :id []
                 :children ({:type :view-3
                             :id [0 :call]})}
               (test-compile [view-1]))))

      (testing "children get sequential ids"

        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [{:children [[view-2]
                                                    [view-2]]}
                                        {:children [[view-2]
                                                    [view-2]]}]})]
          (is (= {:type :view-1,
                  :children [{:children [{:type :view-2,
                                          :id [0 0]}
                                         {:type :view-2,
                                          :id [0 1]}]
                              :id [0]}
                             {:children [{:type :view-2,
                                          :id [1 0]}
                                         {:type :view-2,
                                          :id [1 1]}]
                              :id [1]}]
                  :id []}
                 (test-compile [view-1])))))

      (testing "local ids can be given with :id key"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :layout
                             :children [{:local-id :a
                                         :type :layout
                                         :children [[view-2]]}
                                        {:local-id :b
                                         :type :layout
                                         :children [[view-2]]} ]})]
          (is (= {:type :layout,
                  :id []
                  :children [{:local-id :a
                              :id [:a]
                              :type :layout,
                              :children [{:type :view-2,
                                          :id [:a 0]}]}
                             {:local-id :b
                              :id [:b]
                              :type :layout,
                              :children [{:type :view-2,
                                          :id [:b 0]}]}]}
                 (test-compile [view-1])))))

      (testing "local ids can be given as a metadata to the view call"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [^{:local-id :a} [view-2]
                                        ^{:local-id :b} [view-2]]})]
          (is (= {:type :view-1,
                  :id []
                  :children [{:type :view-2,
                              :local-id :a
                              :id [:a]}
                             {:type :view-2,
                              :local-id :b
                              :id [:b]}]}
                 (test-compile [view-1])))))

      (testing "local ids can be given as a metadata to scene graph node"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :layout
                             :children [^{:local-id :a} {:type :layout
                                                         :children [[view-2]]}
                                        ^{:local-id :b} {:type :layout
                                                         :children [[view-2]]}]})]
          (is (= {:type :layout,
                  :id []
                  :children [{:type :layout,
                              :id [:a]
                              :children [{:type :view-2,
                                          :id [:a 0]}]}
                             {:type :layout,
                              :id [:b]
                              :children [{:type :view-2,
                                          :id [:b 0]}]}]}
                 (test-compile [view-1])))))

      (testing "properties can be given as a metadata to the view call"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [^{:z 1} [view-2]
                                        ^{:z 2} [view-2]]})]
          (is (= {:type :view-1,
                  :id []
                  :children [{:type :view-2,
                              :id [0]
                              :z 1}
                             {:type :view-2,
                              :id [1]
                              :z 2}]}
                 (test-compile [view-1])))))

      (testing "functions in metadata are applied to properties after viewcall is applied"
        (let [view-2 (fn [] {:type :view-2
                             :z 1})
              view-1 (fn [] {:type :view-1
                             :children [^{:z inc} [view-2]]})]
          (is (= {:type :view-1,
                  :id []
                  :children [{:type :view-2,
                              :id [0]
                              :z 2}]}
                 (test-compile [view-1])))))

      (testing "local state"
        (let [handle-mouse-click (fn [count-atom]
                                   (swap! count-atom inc))
              view-2-call-count-atom (atom 0)
              view-2-constructor-call-count-atom (atom 0)
              view-2 (fn []
                       (swap! view-2-constructor-call-count-atom inc)
                       (let [count-atom (dependable-atom/atom 0)]
                         (fn []
                           (swap! view-2-call-count-atom inc)
                           {:count @count-atom
                            :on-mouse-click [handle-mouse-click count-atom]})))
              view-1 (fn [& child-ids]
                       {:children (for [child-id child-ids]
                                    ^{:local-id child-id} [view-2])})]

          (let [scene-graph (test-compile [view-1 :a :b])]
            (is (= 2 (count (keys (:constructor-cache @state)))))

            (is (= [0 0]
                   (->> scene-graph
                        :children
                        (map :count))))

            (is (= [[:a]
                    [:b]]
                   (->> scene-graph
                        :children
                        (map :id))))

            (->> scene-graph
                 :children
                 second
                 :on-mouse-click
                 (callable/call))

            (is (= 2 @view-2-call-count-atom))
            (is (= 2 @view-2-constructor-call-count-atom))

            (is (= [0 1]
                   (->> (test-compile [view-1 :a :b])
                        :children
                        (map :count))))

            (is (= 3 @view-2-call-count-atom))
            (is (= 2 @view-2-constructor-call-count-atom))

            (is (= [1]
                   (->> (test-compile [view-1 :b])
                        :children
                        (map :count))))

            (is (= 3 @view-2-call-count-atom))
            (is (= 2 @view-2-constructor-call-count-atom))

            (test-compile [view-1 :c])

            (is (= 1 (count (keys (:constructor-cache @state)))))

            (is (= 4 @view-2-call-count-atom))
            (is (= 3 @view-2-constructor-call-count-atom))

            (test-compile [view-1])

            (is (= 0 (count (keys (:constructor-cache @state)))))))))))

(deftest test-view-functions
  (with-bindings (merge (state-bindings)
                        (cache/state-bindings))

    (testing ":view-functions"
      (let [view (fn []
                   {:type :view})]
        (is (= {:type :view
                :id []
                :view-functions [{:function view
                                  :dependencies []}]}
               (compile-view-calls [view]))))

      (let [view-1 (fn []
                     {:type :view-1})
            view-2 (fn []
                     [view-1])]
        (is (= {:type :view-1
                :id [:call]
                :view-functions [{:function view-2
                                  :dependencies []}
                                 {:function view-1
                                  :dependencies []}]}
               (compile-view-calls [view-2]))))))

  (testing ":view-functions with dependencies"
    (with-bindings (merge (state-bindings)
                          (cache/state-bindings))
      (let [view (fn []
                   (let [state-atom (dependable-atom/atom "state-atom" 1)]
                     (fn []
                       {:type :view
                        :state @state-atom})))
            scene-graph (compile-view-calls [view])
            state-atom (first (first (get @(:dependencies cache/state)
                                          [(get (:constructor-cache @state)
                                                [])
                                           []])))]

        (is (= {:type :view
                :id []
                :state 1
                :view-functions [{:function view
                                  :dependencies [{:dependable state-atom
                                                  :new-value 1}]}]}
               scene-graph))

        (reset! state-atom 2)

        (is (= {:type :view
                :id []
                :state 2
                :view-functions [{:function view
                                  :dependencies [{:dependable state-atom
                                                  :old-value 1,
                                                  :new-value 2}]}]}
               (compile-view-calls [view])))))


    (with-bindings (merge (state-bindings)
                          (cache/state-bindings))
      (let [state-atom (dependable-atom/atom "state-atom" 1)
            view (fn []
                   {:type :view
                    :state @state-atom})]
        (is (= {:type :view
                :id []
                :state 1
                :view-functions [{:function view
                                  :dependencies [{:dependable state-atom
                                                  :new-value 1}]}]}
               (compile-view-calls [view])))

        (reset! state-atom 2)

        (is (= {:type :view
                :id []
                :state 2
                :view-functions [{:function view
                                  :dependencies [{:dependable state-atom
                                                  :old-value 1,
                                                  :new-value 2}]}]}
               (compile-view-calls [view])))))))


(defn view-functions-to-tree [{:keys [id children view-functions] :as node}]
  (if (not (empty? view-functions))
    (merge {:id id
            :view-function (first view-functions)}
           (when (or children
                     (not (empty? (rest view-functions))))
             {:children (if (empty? (rest view-functions))
                          children
                          [(view-functions-to-tree (update node :view-functions rest))])}))
    node))

(deftest test-view-functions-to-tree
  (is (= {:id 1}
         (view-functions-to-tree {:id 1})))

  (is (= {:id 1,
          :view-function :view-function-1,
          :children [{:id 1
                      :view-function :view-function-2
                      :children [:child]}]}
         (view-functions-to-tree {:id 1
                                  :children [:child]
                                  :view-functions [:view-function-1
                                                   :view-function-2]})))

  (is (= {:id 1, :view-function :view-function-1}
         (view-functions-to-tree {:id 1
                                  :view-functions [:view-function-1]})))


  (is (= {:id 1,
          :view-function :view-function-1,
          :children [{:id 1
                      :view-function :view-function-2}]}
         (view-functions-to-tree {:id 1
                                  :view-functions [:view-function-1 :view-function-2]})))



  (is (= {:id 1,
          :view-function :view-function-1,
          :children [{:id 1,
                      :view-function :view-function-2,
                      :children [{:view-functions [:view-function-3], :id 2}]}]}

         (view-functions-to-tree {:view-functions [:view-function-1 :view-function-2]
                                  :id 1
                                  :children [{:view-functions [:view-function-3]
                                              :id 2}]}))))

(defn component-tree [node]
  (view-functions-to-tree
   (if-let [children (:children node)]
     (assoc node :children (->> children
                                (map component-tree)
                                (mapcat (fn [child]
                                          (if (:view-function child)
                                            [child]
                                            (:children child))))))
     node)))


(deftest test-component-tree-with-fake-scene-graph
  (is (= {:id 1, :view-function :view-function-1}
         (component-tree {:view-functions [:view-function-1]
                          :id 1})))

  (is (= {:id 1,
          :view-function :view-function-1,
          :children [{:id 1
                      :view-function :view-function-2}]}
         (component-tree {:view-functions [:view-function-1
                                           :view-function-2]
                          :id 1})))

  (is (= '{:id 1,
           :view-function :view-function-1,
           :children
           [{:id 1,
             :view-function :view-function-2,
             :children
             ({:id 2,
               :view-function :view-function-1,
               :children [{:id 2, :view-function :view-function-2}]})}]}
         (component-tree {:view-functions [:view-function-1
                                           :view-function-2]
                          :id 1
                          :children [{:view-functions [:view-function-1
                                                       :view-function-2]
                                      :id 2}] })))

  (is (= '{:children
           ({:id 1,
             :view-function :view-function-1,
             :children
             [{:id 1,
               :view-function :view-function-2,
               :children
               ({:id 2,
                 :view-function :view-function-1,
                 :children [{:id 2, :view-function :view-function-2}]})}]})}
         (component-tree {:children [{:view-functions [:view-function-1
                                                       :view-function-2]
                                      :id 1
                                      :children [{:view-functions [:view-function-1
                                                                   :view-function-2]
                                                  :id 2}] }]}))))

(deftest test-component-tree
  (with-bindings (merge (state-bindings)
                        (cache/state-bindings))
    (let [view-2 (fn []
                   (let [state-atom (dependable-atom/atom "view-2 state" 1)]
                     (fn [] {:type :view-2
                             :state @state-atom})))
          view-1 (fn [] {:type :view-1
                         :children [{:type :layout
                                     :children [{:children [[view-2]]}
                                                {:children [[view-2]]}]}]})
          view-state-atom (fn [id]
                            (first (first (get @(:dependencies cache/state)
                                               [(get (:constructor-cache @state)
                                                     id)
                                                []]))))
          component-tree (component-tree (compile-view-calls [view-1]))]

      (is (= {:id [],
              :view-function
              {:function view-1
               :dependencies []},
              :children [{:id [0 0 0],
                          :view-function
                          {:function view-2
                           :dependencies [{:dependable (view-state-atom [0 0 0])
                                           :new-value 1}]}}
                         {:id [0 1 0],
                          :view-function
                          {:function view-2
                           :dependencies [{:dependable (view-state-atom [0 1 0])
                                           :new-value 1}]}}]}

             component-tree)))))

(def label-component ^{:name "label-component" :ns *ns*}
  (fn [label]
    (let [state-atom (dependable-atom/atom {:foo :bar})]
      (fn [label]
        (str label @state-atom)))))

(defn changed-dependency? [dependency-map]
  (and (contains? dependency-map :old-value)
       (not (= (:old-value dependency-map)
               (:new-value dependency-map)))))

(deftest test-changed-dependency?
  (is (changed-dependency? {:old-value 2 :new-value 1}))
  (is (not (changed-dependency? {:new-value 1})))
  (is (not (changed-dependency? {:old-value 1 :new-value 1}))))

(defn describe-value [value]
  (if (< 10 (util/value-size value))
    (hash value)
    (pr-str value)))

(defn describe-dependency-map [dependency-map]
  (str (if (changed-dependency? dependency-map)
         (util/escapes :red)
         "")
       "<" (or (name (:dependable dependency-map))
               (:dependable dependency-map))
       " "
       (hash (:dependable dependency-map))
       " "
       (describe-value (:old-value dependency-map))
       " "
       (describe-value (:new-value dependency-map))
       ">"
       (if (changed-dependency? dependency-map)
         (str "!" (util/escapes :reset))
         "")))


(defn print-component-tree
  ([component-tree]
   (print-component-tree 0 component-tree))

  ([level component-tree]
   (let [view-function-map (:view-function component-tree)]
     (println (string/join " "
                           (concat [(str (apply str (repeat (* level 2) " "))
                                         (when-let [local-id (:local-id view-function-map)]
                                           (str local-id " "))
                                         (or (:name (meta (:function view-function-map)))
                                             (function-class-name-to-function-name (str (:function view-function-map)))))]
                                   (map describe-dependency-map
                                        (:dependencies view-function-map))))))
   (run! (partial print-component-tree (inc level))
         (:children component-tree))))

(deftest test-print-component-tree
  (is (= "view-1
  view-2 view-2-state!
  view-2 view-2-state!
"
         (with-out-str
           (with-bindings (merge (state-bindings)
                                 (cache/state-bindings))
             (let [view-2 ^{:name "view-2"} (fn []
                                              (let [state-atom (dependable-atom/atom "view-2-state" 1)]
                                                (fn [] {:type :view-2
                                                        :state @state-atom})))
                   view-1 ^{:name "view-1"} (fn [] {:type :view-1
                                                    :children [{:type :layout
                                                                :children [{:children [[view-2]]}
                                                                           {:children [[view-2]]}]}]})]


               (compile-view-calls [view-1])
               (compile-view-calls [view-1])
               (run! (fn [dependency]
                       (prn (first dependency) (hash (first dependency)))) (keys @(:dependencies cache/state))) ;; TODO: remove-me

               (print-component-tree (component-tree (compile-view-calls [view-1])))))))))

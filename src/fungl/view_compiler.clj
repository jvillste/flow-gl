(ns fungl.view-compiler
  (:require [fungl.cache :as cache]
            [clojure.test :refer :all]
            [fungl.callable :as callable]
            [fungl.dependable-atom :as dependable-atom]
            [medley.core :as medley]
            [clojure.walk :as walk]
            [fungl.depend :as depend]
            [clojure.string :as string]
            [fungl.util :as util]
            [fungl.id-comparator :as id-comparator]
            (clojure.test.check [clojure-test :as clojure-test]
                                [generators :as generators]
                                [properties :as properties])
            taoensso.tufte))

(def ^:dynamic state)
(def ^:dynamic id)

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

(defn slow-some-is-prefix [sequences sequence]
  (some #(is-prefix sequence %)
        sequences))

;; TODO: make this work using trie
;; https://github.com/chetbox/clj-trie/blob/master/src/clj_trie/core.clj

;; first test if cached-view-call-ids never becomes big enough for
;; slow-some-is-prefix to become a bottleneck

;; using a sorted set does not work because shorter sequences are
;; separated from longer ones, empty sequence is always the first one
;; for example

(defn some-is-prefix [sorted-set-of-sequences sequence]
  (boolean (or (some empty?
                     sorted-set-of-sequences)
               #_(and (= [] sequence)
                      (not (empty? sorted-set-of-sequences)))
               (let [closest-match (first (rsubseq sorted-set-of-sequences
                                                   <=
                                                   sequence))]
                 (if closest-match
                   (if (is-prefix sequence closest-match)
                     true
                     false)
                   nil)))))

(defn run-some-is-prefix-rest [sequences sequence]
  (= (boolean (slow-some-is-prefix sequences sequence))
     (boolean (some-is-prefix (sorted-id-set sequences)
                              sequence))))

(deftest test-some-is-prefix
  (is (run-some-is-prefix-rest (sorted-id-set [[1]])
                               [1]))

  (is (run-some-is-prefix-rest (sorted-id-set [[1]])
                               [1 1]))

  (is (run-some-is-prefix-rest (sorted-id-set [[1]])
                               [1]))

  (is (run-some-is-prefix-rest (sorted-id-set [[0]
                                               [1]
                                               [2]])
                               [1]))

  (is (run-some-is-prefix-rest (sorted-id-set [[0]
                                               [1]
                                               [2]])
                               [1 1]))

  (is (run-some-is-prefix-rest (sorted-id-set [[0]
                                               [2]])
                               [1 1 ]))

  (is (run-some-is-prefix-rest (sorted-id-set [[0]
                                               [1 1 0]
                                               [1 1]
                                               [2]])
                               [1 1 1])))

(clojure-test/defspec property-tpest-some-is-prefix 1000
  (properties/for-all [prefix-candidates (generators/vector (generators/vector generators/int))
                       sequence (generators/vector generators/int)]
                      (run-some-is-prefix-rest prefix-candidates
                                               sequence)))

(comment
  (property-tpest-some-is-prefix)


  (let [prefixes [[-7] [-7 0]]
        sequence [-7 1]]
    [(slow-some-is-prefix prefixes sequence)
     (some-is-prefix (sorted-id-set prefixes)
                      sequence)])

  (some-is-prefix (sorted-id-set [[0]]) [])


  (run-some-is-prefix-rest [[-7] [-7 0]]
                           [-7 1])

  (run-some-is-prefix-rest [[]
                            [0]]
                           [1])
  (sorted-id-set [[]
                  [0]
                  [1]])
  ) ;; TODO: remove me

;; (defn topmost-node-ids [node-ids]
;;   (let [all-sorted-node-ids (sort-by identity
;;                                      id-comparator/compare-ids
;;                                      node-ids)]
;;     (loop [remaining-sorted-node-ids (rest all-sorted-node-ids)
;;            previous-node-id (first all-sorted-node-ids)
;;            topmost-node-ids [(first all-sorted-node-ids)]]

;;       (if (empty? remaining-sorted-node-ids)
;;         topmost-node-ids
;;         (let [next-node-id (first remaining-sorted-node-ids)]
;;           (if (is-prefix next-node-id previous-node-id)
;;             (recur (rest remaining-sorted-node-ids)
;;                    next-node-id
;;                    topmost-node-ids)
;;             (recur (rest remaining-sorted-node-ids)
;;                    next-node-id
;;                    (conj topmost-node-ids
;;                          next-node-id))))))))

;; (deftest test-topmost-node-ids
;;   (is (= [[]]
;;          (topmost-node-ids [[]
;;                             [1]
;;                             [1 2]])))
;;   (is (= [[1] [2]]
;;          (topmost-node-ids [[1]
;;                             [1 2]
;;                             [2]]))))

(defn- view-call? [value]
  (or (and (vector? value)
           (or (fn? (first value))
               (var? (first value))))
      (and (map? value)
           (:view-call value))))

(defn- scene-graph? [value]
  (map? value))

(defn view-call-function-and-arguments [view-call]
  (if (vector? view-call)
    view-call
    (:view-call view-call)))

(defn- apply-view-call [the-id view-call]
  (binding [id the-id]
    (let [[view-function-or-constructor & arguments] (view-call-function-and-arguments view-call)
          scene-graph-or-view-call (cond (apply cache/cached?
                                                view-function-or-constructor
                                                arguments)
                                         (apply cache/call!
                                                view-function-or-constructor
                                                arguments)

                                         (contains? (:constructor-cache @state)
                                                    [the-id view-function-or-constructor])
                                         (let [view-function (get (:constructor-cache @state)
                                                                  [the-id view-function-or-constructor])]
                                           (apply cache/call! view-function arguments))

                                         :else
                                         (let [{:keys [result dependencies]} (cache/call-and-return-result-and-dependencies view-function-or-constructor arguments)]
                                           (assert (some? result)
                                                   "View call must not return nil")
                                           (if (fn? result)
                                             (do (swap! state
                                                        update
                                                        :constructor-cache
                                                        assoc
                                                        [the-id view-function-or-constructor]
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
                (assoc :id the-id
                       :view-function view-function-or-constructor))

            :else
            (throw (Exception. (str "View function did not return a hash map: " view-function-or-constructor)))))))

(defn- apply-metadata [metadata compiled-node]
  (if metadata
    (reduce (fn [node key]
              (if-let [metadata-value (get metadata key)]
                (do #_(when (and (some? (get node key))
                                 (not (= (get node key)
                                         metadata-value)))
                        (println "WARNING: metadata is overriding key" key "in node" (:id compiled-node)))
                    (assoc node key metadata-value))
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

(defn meta-node? [node]
  (and (map? node)
       (:node node)))

(defn compile-node [parent-is-view-call? id value]
  (assert (bound? #'state)
          "Bindings returned by (state-bindings) should be bound.")

  (assert (not (integer? (:local-id (meta value))))
          "local ids can not be integers")

  (cond (meta-node? value)
        (apply-metadata (dissoc value :node)
                        (compile-node parent-is-view-call? id (:node value)))

        (view-call? value)
        (let [[view-function & arguments] (view-call-function-and-arguments value)]
          (apply-metadata (if (vector? value)
                            (meta value)
                            (dissoc value :view-call))
                          (let [id (if parent-is-view-call?
                                     (vec (conj id
                                                (or (:local-id (or (:local-id (meta value))
                                                                   (:local-id value)))
                                                    :call)))
                                     id)
                                ;; _ (prn '(keys (:scene-graph-cache @state)) (keys (:scene-graph-cache @state)))
                                ;;  _ (prn 'cache-keys (keys (:scene-graph-cache @state))) ;; TODO: remove me
                                scene-graph (if-let [ ;; Scene graph cache is disabled. It did not work for command help in argupedia ui.
                                                     ;; I did not try to debug it since the performance is ok now without it.
                                                     scene-graph #_nil (get (:scene-graph-cache @state)
                                                                            [id view-function arguments])]



                                              (do
                                                #_(prn 'component-cache-hit!
                                                       (function-name (if (var? (first value))
                                                                        @(first value)
                                                                        (first value)))
                                                       id) ;; TODO: remove me

                                                #_(println "found from cache " [id view-function arguments] (System/identityHashCode scene-graph))

                                                (swap! state
                                                       update
                                                       :view-call-ids-from-cache
                                                       conj
                                                       [id view-function])

                                                scene-graph)
                                              (do #_(prn 'component-cache-miss
                                                         (function-name (if (var? (first value))
                                                                          @(first value)
                                                                          (first value)))
                                                         id)
                                                  (swap! state
                                                         update
                                                         :applied-view-call-ids
                                                         conj
                                                         [id view-function])

                                                  (swap! state
                                                         update
                                                         :applied-view-calls
                                                         conj
                                                         [id view-function arguments])


                                                  (let [scene-graph (-> (compile-node true
                                                                                      id
                                                                                      (apply-view-call id value))
                                                                        (assoc :view-call? true))]

                                                    ;;                                                    (println "adding to cache " [id view-function] (System/identityHashCode scene-graph))

                                                    #_(prn 'saving-scene-graph [id view-function arguments]) ;; TODO: remove me

                                                    (swap! state
                                                           update
                                                           :scene-graph-cache
                                                           assoc
                                                           [id view-function arguments]
                                                           scene-graph)

                                                    scene-graph)))

                                dependency-value-map (cache/dependency-value-map (or (get (:constructor-cache @state)
                                                                                          [id view-function])
                                                                                     view-function)
                                                                                 arguments)
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
                                     [id view-function]
                                     dependency-value-map))



                            (swap! state
                                   update
                                   :view-functions
                                   assoc
                                   [id view-function]
                                   view-function)

                            scene-graph)))

        (:children value)
        (-> value
            (update :children
                    (fn [children]
                      (vec (map-indexed (fn [index child]
                                          (assert (nil? (:local-id (meta child)))
                                                  "Using metadata is deprecated. Use {:local-id 1 :node node} i.e. meta nodes instead.")
                                          (let [compiled-node (compile-node false
                                                                            (vec (conj id
                                                                                       (or (:local-id child)
                                                                                           index)))
                                                                            child)]
                                            ;;                                            (prn 'compiled-node (System/identityHashCode compiled-node)) ;; TODO: remove me
                                            compiled-node))
                                        children))))
            (assoc :id id))

        :else
        (assoc value
               :id id)))

(defn invalidated-node-ids [node-dependencies]
  (->> node-dependencies
       (filter (fn [[_node-id dependency-value-map]]
                 (some (fn [[dependency value]]
                         (not (identical? value
                                          (depend/current-value dependency))))
                       dependency-value-map)))
       (map first)))

(defn invalidated? [node-id]
  (is-prefix-of-some (:sorted-invalidated-node-ids-set @state) node-id))

(defn start-compilation-cycle! []
  (let [sorted-invalidated-node-ids-set (sorted-id-set (map first (invalidated-node-ids (:node-dependencies @state))))
        remove-invalidated-view-call-ids (partial medley/remove-keys
                                                  (fn [[id _view-function]]
                                                    (is-prefix-of-some sorted-invalidated-node-ids-set
                                                                       id)))]

    ;;    (prn 'sorted-invalidated-node-ids-set sorted-invalidated-node-ids-set) ;; TODO: remove me

    (swap! state
           (fn [state]
             (-> state
                 (update :node-dependencies remove-invalidated-view-call-ids)
                 (update :scene-graph-cache remove-invalidated-view-call-ids)
                 (assoc :applied-view-call-ids #{})
                 (assoc :applied-view-calls #{})
                 (assoc :view-call-ids-from-cache #{})
                 (assoc :sorted-invalidated-node-ids-set sorted-invalidated-node-ids-set))))
    #_(prn :scene-graph-cache (keys (:scene-graph-cache @state))) ;; TODO: remove me
    ))

(defn end-compilation-cycle! []
  ;; (prn '(:applied-view-call-ids @state) (:applied-view-call-ids @state)) ;; TODO: remove me
  ;; (prn '(:view-call-ids-from-cache @state) (:view-call-ids-from-cache @state)) ;; TODO: remove me
  ;; (prn ":scene-graph-cache after compilation: " (keys (:scene-graph-cache @state))) ;; TODO: remove me

  (let [sorted-cached-node-id-set (sorted-id-set (map first (:view-call-ids-from-cache @state)))
        remove-unused-view-call-ids (partial medley/filter-keys
                                             (fn [view-call-id]
                                               (if (or (contains? (:applied-view-call-ids @state)
                                                                  view-call-id)
                                                       (slow-some-is-prefix sorted-cached-node-id-set
                                                                            (first view-call-id)))
                                                 true
                                                 (do #_(println "removing unused view call id" view-call-id)
                                                     false))))

        remove-unused-view-calls (partial medley/filter-keys
                                          (fn [view-call]
                                            (if (or (contains? (:applied-view-calls @state)
                                                               view-call)
                                                    (slow-some-is-prefix sorted-cached-node-id-set
                                                                         (first view-call)))
                                              true
                                              (do #_(println "removing unused view call" view-call)
                                                  false))))]

    (swap! state
           (fn [state]
             (-> state
                 (update :scene-graph-cache remove-unused-view-calls)
                 (update :view-functions remove-unused-view-call-ids)
                 (update :node-dependencies remove-unused-view-call-ids)
                 (update :constructor-cache remove-unused-view-call-ids))))))

(defn compile-view-calls [view-call-or-scene-graph]
  (compile-node false
                []
                view-call-or-scene-graph))

(defn state-bindings []
  {#'state (atom {:applied-view-call-ids #{}
                  :applied-view-calls #{}
                  :view-call-ids-from-cache #{}
                  :constructor-cache {}
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
                :id []
                :view-call? true}
               (test-compile [view])))

        (is (= {:type :view
                :id []
                :view-call? true}
               (test-compile {:view-call [view]}))))

      (testing "if a view only calls another view, they must have different ids to differentiate their state. This is why there is a :call -keyword inserted into the id"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] [view-2])]
          (is (= {:type :view-2
                  :id [:call]
                  :view-call? true}
                 (test-compile [view-1])))))


      (let [view-2 (fn [] {:type :view-2})
            view-1 (fn [] {:type :view-1
                           :children [[view-2]]})]
        (is (= '{:type :view-1
                 :id []
                 :view-call? true
                 :children ({:type :view-2
                             :id [0]
                             :view-call? true})}
               (test-compile [view-1]))))

      (let [view-3 (fn [] {:type :view-3})
            view-2 (fn [] [view-3])
            view-1 (fn [] {:type :view-1
                           :children [[view-2]]})]
        (is (= '{:type :view-1
                 :id []
                 :view-call? true
                 :children ({:type :view-3
                             :id [0 :call]
                             :view-call? true})}
               (test-compile [view-1]))))

      (testing "children get sequential ids"

        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [{:children [[view-2]
                                                    [view-2]]}
                                        {:children [[view-2]
                                                    [view-2]]}]})]
          (is (= {:type :view-1,
                  :view-call? true
                  :children [{:children [{:type :view-2,
                                          :id [0 0]
                                          :view-call? true}
                                         {:type :view-2,
                                          :id [0 1]
                                          :view-call? true}]
                              :id [0]}
                             {:children [{:type :view-2,
                                          :id [1 0]
                                          :view-call? true}
                                         {:type :view-2,
                                          :id [1 1]
                                          :view-call? true}]
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
                  :view-call? true
                  :children [{:local-id :a
                              :id [:a]
                              :type :layout,
                              :children [{:type :view-2,
                                          :id [:a 0]
                                          :view-call? true}]}
                             {:local-id :b
                              :id [:b]
                              :type :layout,
                              :children [{:type :view-2,
                                          :id [:b 0]
                                          :view-call? true}]}]}
                 (test-compile [view-1])))))

      (testing "local ids can be given as a metadata to the view call"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [^{:local-id :a} [view-2]
                                        ^{:local-id :b} [view-2]]})]
          (is (= {:type :view-1,
                  :id []
                  :view-call? true
                  :children [{:type :view-2,
                              :local-id :a
                              :id [:a]
                              :view-call? true}
                             {:type :view-2,
                              :local-id :b
                              :id [:b]
                              :view-call? true}]}
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
                  :view-call? true
                  :children [{:type :layout,
                              :id [:a]
                              :children [{:type :view-2,
                                          :id [:a 0]
                                          :view-call? true}]}
                             {:type :layout,
                              :id [:b]
                              :children [{:type :view-2,
                                          :id [:b 0]
                                          :view-call? true}]}]}
                 (test-compile [view-1])))))

      (testing "properties can be given as a metadata to the view call"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [^{:z 1} [view-2]
                                        ^{:z 2} [view-2]]})]
          (is (= {:type :view-1,
                  :id []
                  :view-call? true
                  :children [{:type :view-2,
                              :id [0]
                              :view-call? true
                              :z 1}
                             {:type :view-2,
                              :id [1]
                              :view-call? true
                              :z 2}]}
                 (test-compile [view-1])))))

      (testing "properties can be given in view call map"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [{:z 1
                                         :view-call [view-2]}
                                        {:z 2
                                         :view-call [view-2]}]})]
          (is (= {:type :view-1,
                  :id []
                  :view-call? true
                  :children [{:type :view-2,
                              :id [0]
                              :view-call? true
                              :z 1}
                             {:type :view-2,
                              :id [1]
                              :view-call? true
                              :z 2}]}
                 (test-compile [view-1]))))))))

(deftest test-local-state
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

        (is (= 0 (count (keys (:constructor-cache @state)))))))))

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

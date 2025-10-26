(ns fungl.view-compiler
  (:require
   [clojure.string :as string]
   [clojure.test :refer :all]
   (clojure.test.check [clojure-test :as clojure-test]
                                [generators :as generators]
                                [properties :as properties])
   [clojure.walk :as walk]
   [fungl.cache :as cache]
   [fungl.callable :as callable]
   [fungl.depend :as depend]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
   [fungl.id-comparator :as id-comparator]
   [fungl.log :as log]
   [fungl.util :as util]
   taoensso.tufte))

(def ^:dynamic compile-node-cache-atom)

(def ^:dynamic id)
(def ^:dynamic compilation-path)

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
  (and (vector? value)
       (or (fn? (first value))
           (var? (first value)))))

(defn- scene-graph? [value]
  (map? value))


(defn- apply-view-call [the-id compilation-path view-call]
  ;; (when-some [size (some (fn [argument]
  ;;                          (when (coll? argument)
  ;;                            (let [size (util/value-size argument)]
  ;;                              (when (< 10 (util/value-size argument))
  ;;                                size))))
  ;;                        (rest view-call))]
  ;;   (println "WARNING: large argument given for view call. Give children as view calls to avoid time consuming argument comparisons."
  ;;            (util/fully-qualified-function-name (first view-call))
  ;;            size
  ;;            the-id))
  (binding [id the-id
            compilation-path compilation-path]
    (let [[view-function-or-constructor & arguments] view-call
          scene-graph-or-view-call (if (hierarchical-identity-cache/cached-call? compile-node-cache-atom
                                                                                 compilation-path
                                                                                 0
                                                                                 view-function-or-constructor
                                                                                 arguments)

                                     (do (log/write "view-function was cached"
                                                    (util/fully-qualified-function-or-var-name (first view-call))
                                                    compilation-path)

                                         ;; cached view function

                                         (apply hierarchical-identity-cache/call-with-cache
                                                compile-node-cache-atom
                                                compilation-path
                                                0
                                                view-function-or-constructor
                                                arguments))

                                     (let [cached-value (hierarchical-identity-cache/get-value-with-path-only compile-node-cache-atom
                                                                                                              compilation-path
                                                                                                              [:constructor view-function-or-constructor])]
                                       (if (= ::hierarchical-identity-cache/not-found cached-value)

                                         ;; the given function was not a cached constructor

                                         (let [{:keys [result dependencies]} (depend/call-and-return-result-and-dependencies view-function-or-constructor
                                                                                                                             arguments)]


                                           (if (fn? result)

                                             ;; the given function was a constructor and returend a view function

                                             (do (log/write "ran constructor"
                                                            (util/fully-qualified-function-or-var-name (first view-call))
                                                            compilation-path)
                                                 (hierarchical-identity-cache/add-to-cache! compile-node-cache-atom
                                                                                            compilation-path
                                                                                            [:constructor view-function-or-constructor]
                                                                                            []
                                                                                            []
                                                                                            result
                                                                                            [])
                                                 (apply hierarchical-identity-cache/call-with-cache
                                                        compile-node-cache-atom
                                                        compilation-path
                                                        0
                                                        result
                                                        arguments))

                                             ;; the given function was a view function

                                             (do (log/write "called view-function"
                                                            (util/fully-qualified-function-or-var-name (first view-call))
                                                            compilation-path)
                                                 (hierarchical-identity-cache/add-to-cache! compile-node-cache-atom
                                                                                            compilation-path
                                                                                            view-function-or-constructor
                                                                                            []
                                                                                            arguments
                                                                                            result
                                                                                            dependencies)
                                                 result)))

                                         ;; view function was found from constructor cache

                                         (do (log/write "running view function from constructor cache"
                                                        (util/fully-qualified-function-or-var-name cached-value)
                                                        compilation-path)
                                             (apply hierarchical-identity-cache/call-with-cache
                                                    compile-node-cache-atom
                                                    compilation-path
                                                    0
                                                    cached-value
                                                    arguments)))))]

      (cond (view-call? scene-graph-or-view-call)
            scene-graph-or-view-call

            (scene-graph? scene-graph-or-view-call)
            (-> scene-graph-or-view-call
                (assoc :id the-id))

            :else
            (throw (Exception. (str "View function did not return a hash map nor a view call: " view-function-or-constructor)))))))

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

(defn meta-node? [node]
  (and (map? node)
       (:node node)))

(declare compile-node)

(defn call-compile-node-with-cache [id compilation-path value]
  (hierarchical-identity-cache/call-with-cache compile-node-cache-atom
                                               compilation-path
                                               -1
                                               compile-node
                                               id
                                               compilation-path
                                               value))

(defn compile-node [id compilation-path value]
  (assert (not (integer? (:local-id (meta value))))
          "local ids can not be integers")

  (assert (not (and (map? value)
                    (:view-call value)))
          (str "View calls as map are deprecated, use a vector and a meta node instead. Path was: " compilation-path))

  (cond (meta-node? value)
        (let [meta-node-compilation-path (conj compilation-path :meta-node)]
          (hierarchical-identity-cache/call-with-cache compile-node-cache-atom
                                                       meta-node-compilation-path
                                                       -1
                                                       apply-metadata
                                                       (dissoc value :node)
                                                       (call-compile-node-with-cache id
                                                                                     meta-node-compilation-path
                                                                                     (:node value))))

        (view-call? value)
        (let [view-call-compilation-path (conj compilation-path :view-call)]
          (call-compile-node-with-cache id
                                        view-call-compilation-path
                                        (hierarchical-identity-cache/call-with-cache compile-node-cache-atom
                                                                                     view-call-compilation-path
                                                                                     0
                                                                                     apply-view-call
                                                                                     id
                                                                                     view-call-compilation-path
                                                                                     value)))

        (:children value)
        (-> value
            (update :children
                    (fn [children]
                      (when (= clojure.lang.LazySeq (type children))
                        (throw (Exception. (str "Scene graph node children can not be a lazy seq for id binding to work. Use doall or vec to force realization during view call. compilation path was: " compilation-path " node type was " (:type value)))))

                      (vec (map-indexed (fn [index child]
                                          (assert (nil? (:local-id (meta child)))
                                                  (str "Using metadata is deprecated. Use {:local-id 1 :node node} i.e. meta nodes instead. local-id was: " (:local-id (meta child))))

                                          (call-compile-node-with-cache (vec (conj id
                                                                                   (or (:local-id child)
                                                                                       index)))
                                                                        (vec (conj compilation-path
                                                                                   (or (:local-id child)
                                                                                       index)))
                                                                        child))
                                        children))))
            (assoc :id id
                   :compilation-path compilation-path))

        :else
        (assoc value
               :id id
               :compilation-path compilation-path)))

(defn invalidated-node-ids [node-dependencies]
  (->> node-dependencies
       (filter (fn [[_node-id dependency-value-map]]
                 (some (fn [[dependency value]]
                         (not (identical? value
                                          (depend/current-value dependency))))
                       dependency-value-map)))
       (map first)))


(defn compile-view-calls [view-call-or-scene-graph]
  ;; component state must be cleared after every other frame so that if
  ;; the compnent with the same id is created again, its state is reset
  (binding [hierarchical-identity-cache/maximum-number-of-cycles-without-removing-unused-keys 1]
    (hierarchical-identity-cache/with-cache-cleanup compile-node-cache-atom


      ;; (def invalid (->> (hierarchical-identity-cache/dependenciy-nodes @compile-node-cache-atom)
      ;;                   (hierarchical-identity-cache/filter-invalid-dependencies)
      ;;                   (sort-by (comp count :path))
      ;;                   (first)))

      ;; (println "invalid dependencies:")
      ;; (->> (hierarchical-identity-cache/dependency-nodes @compile-node-cache-atom)
      ;;      (hierarchical-identity-cache/filter-invalid-dependencies)
      ;;      (sort-by (comp count :path))
      ;;      (hierarchical-identity-cache/print-dependency-nodes))

      (let [result (call-compile-node-with-cache []
                                                 []
                                                 view-call-or-scene-graph)]
        ;; (println)
        ;; (hierarchical-identity-cache/print-statistics compile-node-cache-atom)
        result))))

(defn state-bindings []
  {#'compile-node-cache-atom (hierarchical-identity-cache/create-cache-atom "compile-node")})

(deftest test-compile
  (let [test-compile (fn [value]
                       (walk/postwalk (fn [node]
                                        (if (map? node)
                                          (dissoc node
                                                  :render
                                                  :view-functions
                                                  :view-function
                                                  :compilation-path)
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


      (let [view (fn []
                   {:type :view})
            scene-graph (call-compile-node-with-cache [] [] [view])]
        (is (= {:type :view
                :id []
                :compilation-path [:view-call]}
               scene-graph))
        (is (identical? scene-graph
                        (call-compile-node-with-cache [] [] [view]))))

      (testing "if a view only calls another view, they must have different ids to differentiate their state. This is why there is a :call -keyword inserted into the id"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] [view-2])]
          (is (= {:type :view-2
                  :id []}
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
                             :id [0]})}
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

      (testing "properties can be given in view call map"
        (let [view-2 (fn [] {:type :view-2})
              view-1 (fn [] {:type :view-1
                             :children [{:z 1
                                         :node [view-2]}
                                        {:z 2
                                         :node [view-2]}]})]
          (is (= {:type :view-1,
                  :id []
                  :children [{:type :view-2,
                              :id [0]
                              :z 1}
                             {:type :view-2,
                              :id [1]
                              :z 2}]}
                 (test-compile [view-1])))))

      (testing "state"
        (let [state-atom (dependable-atom/atom 0)
              view-2-call-count-atom (atom 0)
              view-2 (fn [] (swap! view-2-call-count-atom inc)
                       {:type :view-2
                        :state @state-atom})
              view-1 (fn [] {:type :view-1
                             :children [[view-2]]})]
          (is (= {:type :view-1,
                  :children [{:type :view-2, :state 0, :id [0]}],
                  :id []}
                 (test-compile [view-1])))
          (is (= 1 @view-2-call-count-atom))
          (is (= {:type :view-1,
                  :children [{:type :view-2, :state 0, :id [0]}],
                  :id []}
                 (test-compile [view-1])))
          (is (= 1 @view-2-call-count-atom))
          (swap! state-atom inc)
          (is (= {:type :view-1,
                  :children [{:type :view-2, :state 1, :id [0]}],
                  :id []}
                 (test-compile [view-1])))
          (is (= 2 @view-2-call-count-atom))
          (swap! state-atom inc)
          (is (= {:type :view-1,
                  :children [{:type :view-2, :state 2, :id [0]}],
                  :id []}
                 (test-compile [view-1])))))



      (testing "two states"
        (let [state-2-atom (dependable-atom/atom 0)
              state-3-atom (dependable-atom/atom 0)
              view-2-call-count-atom (atom 0)
              view-2 (fn [] (swap! view-2-call-count-atom inc)
                       {:type :view-2
                        :state @state-2-atom})
              view-3-call-count-atom (atom 0)
              view-3 (fn []
                       (swap! view-3-call-count-atom inc)
                       {:type :view-3
                        :state @state-3-atom})
              view-1 (fn [] {:type :view-1
                             :children [[view-2]
                                        [view-3]]})]
          (is (= {:type :view-1,
                  :children
                  [{:type :view-2, :state 0, :id [0]}
                   {:type :view-3, :state 0, :id [1]}],
                  :id []}
                 (test-compile [view-1])))
          (is (= 1 @view-2-call-count-atom))
          (is (= {:type :view-1,
                  :children
                  [{:type :view-2, :state 0, :id [0]}
                   {:type :view-3, :state 0, :id [1]}],
                  :id []}
                 (test-compile [view-1])))
          (is (= 1 @view-2-call-count-atom))
          (swap! state-2-atom inc)
          (is (= {:type :view-1,
                  :children
                  [{:type :view-2, :state 1, :id [0]}
                   {:type :view-3, :state 0, :id [1]}],
                  :id []}
                 (test-compile [view-1])))
          (is (= 2 @view-2-call-count-atom))
          (is (= 1 @view-3-call-count-atom))

          (swap! state-3-atom inc)
          (is (= {:type :view-1,
                  :children
                  [{:type :view-2, :state 1, :id [0]}
                   {:type :view-3, :state 1, :id [1]}],
                  :id []}
                 (test-compile [view-1])))
          (is (= 2 @view-2-call-count-atom))
          (is (= 2 @view-3-call-count-atom)))))))

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
;;        (is (= 2 (count (keys (:constructor-cache @state)))))

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

;;        (is (= 1 (count (keys (:constructor-cache @state)))))

        (is (= 4 @view-2-call-count-atom))
        (is (= 3 @view-2-constructor-call-count-atom))

        (test-compile [view-1])

;;        (is (= 0 (count (keys (:constructor-cache @state)))))
        ))))

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
                                          [#_(get (:constructor-cache @state)
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
                                               [#_(get (:constructor-cache @state)
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
                                             (util/function-class-name-to-function-name (str (:function view-function-map)))))]
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

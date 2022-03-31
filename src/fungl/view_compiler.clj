(ns fungl.view-compiler
  (:require [fungl.cache :as cache]
            [clojure.test :refer :all]
            [fungl.callable :as callable]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.set :as set]
            [medley.core :as medley]))

(def ^:dynamic state)

(def ^:dynamic id)
(def ^:dynamic used-constructor-ids)

(defn- view-call? [value]
  (and (vector? value)
       (or (fn? (first value))
           (var? (first value)))))

(defn- scene-graph? [value]
  (map? value))

(defn with-constructor-cache-cleanup* [function]
  (binding [used-constructor-ids (atom #{})]
    (let [result (function)]

      (doseq [id (set/difference (set (keys @(:constructor-cache state)))
                                 @used-constructor-ids)]
        (swap! (:constructor-cache state)
               dissoc
               id))

      result)))

(defmacro with-constructor-cache-cleanup [& body]
  `(with-constructor-cache-cleanup* (fn [] ~@body)))

(defn apply-view-call [the-id view-call]
  (assert (bound? #'used-constructor-ids)
          "apply-view-call must be called inside with-constructor-cache-cleanup")

  (binding [id the-id]
    (let [[view-function-or-constructor & arguments] view-call
          scene-graph-or-view-call (cond (cache/cached? (cache/function-call-key view-function-or-constructor arguments))
                                         (apply cache/call! view-function-or-constructor arguments)

                                         (contains? @(:constructor-cache state)
                                                    the-id)
                                         (let [view-function (get @(:constructor-cache state)
                                                                  the-id)]
                                           (swap! used-constructor-ids conj the-id)
                                           (apply cache/call! view-function arguments))

                                         :default
                                         (let [view-function-or-scene-graph (apply view-function-or-constructor arguments)]
                                           (if (fn? view-function-or-scene-graph)
                                             (do (swap! (:constructor-cache state)
                                                        assoc
                                                        the-id
                                                        view-function-or-scene-graph)
                                                 (swap! used-constructor-ids conj the-id)

                                                 (apply cache/call! view-function-or-scene-graph arguments))
                                             (do (cache/put! (cache/function-call-key view-function-or-constructor arguments)
                                                             view-function-or-scene-graph)
                                                 view-function-or-scene-graph))))]
      (cond (view-call? scene-graph-or-view-call)
            scene-graph-or-view-call

            (scene-graph? scene-graph-or-view-call)
            (-> scene-graph-or-view-call
                (assoc :id the-id))

            :default
            (throw (Exception. (str "View function did not return a hash map: " view-function-or-constructor)))))))

(defn apply-metadata [metadata compiled-node]
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

(defn compile [parent-is-view? id value]
  (assert (bound? #'state)
          "Bindings returned by (state-bindings) should be bound.")

  (cond (view-call? value)
        (apply-metadata (meta value)
                        (let [id (if parent-is-view?
                                   (vec (conj id
                                              (or (:local-id (meta value))
                                                  :call)))
                                   id)]
                          (compile true
                                   id
                                   (apply-view-call id value))))

        (:children value)
        (-> value
            (update :children
                    (fn [children]
                      (vec (map-indexed (fn [index child]
                                          (compile false
                                                   (vec (conj id
                                                              (or (:local-id (meta child))
                                                                  (:local-id child)
                                                                  index)))
                                                   child))
                                        children))))
            (assoc :id id))

        :default
        (assoc value :id id)))



(defn- run-compile [view-call-or-scene-graph]


  (with-constructor-cache-cleanup (compile false
                                           []
                                           view-call-or-scene-graph)))

(defn state-bindings []
  {#'state {:constructor-cache (atom {})
            :used-constructors (atom #{})}})

(deftest test-compile
  (with-bindings (merge (state-bindings)
                        (cache/state-bindings))

    (is (= {:type :root
            :id []}
           (run-compile {:type :root})))

    (let [view (fn []
                 {:type :view})]
      (is (= {:type :view
              :id []}
             (run-compile [view]))))

    (testing "if a view only calls another view, they must have different ids to differentiate their state. This is why there is a :call -keyword inserted into the id"
      (let [view-2 (fn [] {:type :view-2})
            view-1 (fn [] [view-2])]
        (is (= {:type :view-2
                :id [:call]}
               (run-compile [view-1])))))


    (let [view-2 (fn [] {:type :view-2})
          view-1 (fn [] {:type :view-1
                         :children [[view-2]]})]
      (is (= '{:type :view-1
               :id []
               :children ({:type :view-2
                           :id [0]})}
             (run-compile [view-1]))))

    (let [view-3 (fn [] {:type :view-3})
          view-2 (fn [] [view-3])
          view-1 (fn [] {:type :view-1
                         :children [[view-2]]})]
      (is (= '{:type :view-1
               :id []
               :children ({:type :view-3
                           :id [0 :call]})}
             (run-compile [view-1]))))

    ;; (testing "children get sequential ids"

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
             (run-compile [view-1]))))

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
               (run-compile [view-1])))))

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
               (run-compile [view-1])))))

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
               (run-compile [view-1])))))

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
               (run-compile [view-1])))))

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
               (run-compile [view-1])))))

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

        (let [scene-graph (run-compile [view-1 :a :b])]
          (is (= 2 (count (keys @(:constructor-cache state)))))

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
                 (->> (run-compile [view-1 :a :b])
                      :children
                      (map :count))))

          (is (= 3 @view-2-call-count-atom))
          (is (= 2 @view-2-constructor-call-count-atom))

          (is (= [1]
                 (->> (run-compile [view-1 :b])
                      :children
                      (map :count))))

          (is (= 3 @view-2-call-count-atom))
          (is (= 2 @view-2-constructor-call-count-atom))

          (run-compile [view-1 :c])

          (is (= 1 (count (keys @(:constructor-cache state)))))

          (is (= 4 @view-2-call-count-atom))
          (is (= 3 @view-2-constructor-call-count-atom))

          (run-compile [view-1])

          (is (= 0 (count (keys @(:constructor-cache state))))))))))

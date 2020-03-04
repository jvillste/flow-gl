(ns fungl.view-compiler
  (:require [fungl.cache :as cache]
            [clojure.test :refer :all]
            [fungl.callable :as callable]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.set :as set]))

(def ^:dynamic state)

(def ^:dynamic id)
(def ^:dynamic used-constructor-ids)

(defn- view-call? [value]
  (and (vector? value)
       (fn? (first value))))

(defn- scene-graph? [value]
  (map? value))

(defn apply-view-call [the-id view-call]
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

(defn- compile* [parent-id value]
  (cond (view-call? value)
        (let [id (conj parent-id (first value))]
          (compile* id
                    (apply-view-call id value)))

        (:children value)
        (update value :children
                (fn [children]
                  (doall (map-indexed (fn [index child]
                                        (compile* (conj parent-id
                                                        (or (:id (meta child))
                                                            index))
                                                  child))
                                      children))))

        :default
        value))

(defn compile [view-call-or-scene-graph]
  (assert (bound? #'state)
          "Bindings returned by (state-bindings) should be bound.")

  (binding [used-constructor-ids (atom #{})]
    (let [scene-graph  (compile* [] view-call-or-scene-graph)]

      (doseq [id (set/difference (set (keys @(:constructor-cache state)))
                                 @used-constructor-ids)]
        (swap! (:constructor-cache state)
               dissoc
               id))

      scene-graph)))

(defn state-bindings []
  {#'state {:constructor-cache (atom {})
            :used-constructors (atom #{})}})

(deftest test-compile
  (with-bindings (merge (state-bindings)
                        (cache/state-bindings))

    (is (= {} (compile {})))

    (let [view (fn []
                 {:type :view})]
      (is (= {:type :view
              :id [view]}
             (compile [view]))))

    (let [view-2 (fn [] {:type :view-2})
          view-1 (fn [] [view-2])]
      (is (= {:type :view-2
              :id [view-1 view-2]}
             (compile [view-1]))))

    (testing "children get sequential ids"

      (let [view-2 (fn [] {:type :view-2})
            view-1 (fn [] {:type :view-1
                           :children [{:children [[view-2]
                                                  [view-2]]}
                                      {:children [[view-2]
                                                  [view-2]]}]})]
        (is (= {:type :view-1,
                :children [{:children [{:type :view-2,
                                        :id [view-1 0 0 view-2]}
                                       {:type :view-2,
                                        :id [view-1 0 1 view-2]}]}
                           {:children [{:type :view-2,
                                        :id [view-1 1 0 view-2]}
                                       {:type :view-2,
                                        :id [view-1 1 1 view-2]}]}]
                :id [view-1]}
               (compile [view-1])))))

    (testing "local ids can be given as a metadata to the view call"
      (let [view-2 (fn [] {:type :view-2})
            view-1 (fn [] {:type :view-1
                           :children [^{:id :a} [view-2]
                                      ^{:id :b} [view-2]]})]
        (is (= {:type :view-1,
                :id [view-1]
                :children [{:type :view-2,
                            :id [view-1 :a view-2]}
                           {:type :view-2,
                            :id [view-1 :b view-2]}]}
               (compile [view-1])))))

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
                                  ^{:id child-id} [view-2])})]

        (let [scene-graph (compile [view-1 :a :b])]
          (is (= 2 (count (keys @(:constructor-cache state)))))

          (is (= [0 0]
                 (->> scene-graph
                      :children
                      (map :count))))

          (is (= [[view-1 :a view-2]
                  [view-1 :b view-2]]
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
                 (->> (compile [view-1 :a :b])
                      :children
                      (map :count))))

          (is (= 3 @view-2-call-count-atom))
          (is (= 2 @view-2-constructor-call-count-atom))

          (is (= [1]
                 (->> (compile [view-1 :b])
                      :children
                      (map :count))))

          (is (= 3 @view-2-call-count-atom))
          (is (= 2 @view-2-constructor-call-count-atom))

          (compile [view-1 :c])

          (is (= 1 (count (keys @(:constructor-cache state)))))

          (is (= 4 @view-2-call-count-atom))
          (is (= 3 @view-2-constructor-call-count-atom))

          (compile [view-1])

          (is (= 0 (count (keys @(:constructor-cache state))))))))))

(ns fungl.view-compiler
  (:require [fungl.cache :as cache]
            [clojure.test :refer :all]
            [fungl.callable :as callable]))

(def ^:dynamic view-function-cache-atom)

(defn- view-call? [value]
  (and (vector? value)
       (fn? (first value))))

(defn apply-view-call [id view-call]
  (let [[view-function & arguments] view-call
        scene-graph-or-view-call (if-let [cached-view-function (get @view-function-cache-atom id)]
                                   (apply cache/call! cached-view-function arguments)
                                   (let [scene-graph-or-view-function (apply cache/call! view-function arguments)]
                                     (if (fn? scene-graph-or-view-function)
                                       (do (swap! view-function-cache-atom assoc id scene-graph-or-view-function)
                                           (apply cache/call! scene-graph-or-view-function arguments))
                                       scene-graph-or-view-function)))]
    (if (view-call? scene-graph-or-view-call)
      scene-graph-or-view-call
      (-> scene-graph-or-view-call
          (assoc :id id)))))

(defn- compile* [parent-id value]
  (cond (view-call? value)
        (let [id (conj parent-id (first value))]
          (compile* id
                    (apply-view-call id value)))

        (:children value)
        (update value :children
                (fn [children]
                  (map-indexed (fn [index child]
                                 (compile* (conj parent-id
                                                 (or (:id (meta child))
                                                     index))
                                           child))
                               children)))

        :default
        value))

(defn compile [view-call-or-scene-graph]
  (compile* [] view-call-or-scene-graph))

(defn state-bindings []
  {#'view-function-cache-atom (atom {})})

(deftest test-compile
  (with-bindings (merge (state-bindings))

    (is (= {} (compile {})))

    (let [view (fn [] {:type :view})]
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
            view-2 (fn [] (let [count-atom (atom 0)]
                            (fn [] {:count @count-atom
                                    :on-mouse-click [handle-mouse-click count-atom]})))
            view-1 (fn [& child-ids]
                     {:children (for [child-id child-ids]
                                  ^{:id child-id} [view-2])})]

        (let [scene-graph (compile [view-1 :a :b])]
          (is (= [0 0]
                 (->> scene-graph
                      :children
                      (map :count))))

          (->> scene-graph
               :children
               second
               :on-mouse-click
               (callable/call))

          (is (= [0 1]
                 (->> (compile [view-1 :a :b])
                      :children
                      (map :count))))

          (is (= [1]
                 (->> (compile [view-1 :b])
                      :children
                      (map :count)))))))))

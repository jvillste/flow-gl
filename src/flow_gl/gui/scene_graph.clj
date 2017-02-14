(ns flow-gl.gui.scene-graph
  (:require [fungl.cache :as cache]
            [clojure.spec :as spec]
            [clojure.test :as test :refer [deftest is]]))

#_(defprotocol Node
    (children [this]))

#_(extend Object
    Node {::children (fn [this] (::children this))})

(spec/def ::coordinate number?)
(spec/def ::x ::coordinate)
(spec/def ::y ::coordinate)
(spec/def ::z ::coordinate)
(spec/def ::width number?)
(spec/def ::height number?)
(spec/def ::children (spec/* ::node))

(spec/def ::node (spec/keys :req-un []
                            :opt-un [::x ::y ::z ::children]))

(spec/def ::layouted-children (spec/* ::layouted-node))
(spec/def ::layouted-node (spec/merge ::node
                                      (spec/keys :req-un [::width ::height]
                                                 :opt-un [::layouted-children])))


#_(spec/explain ::node {::x 1 ::y 1})

(defn leaf-nodes
  "Returns leave nodes in depth first and ascending z order with global x and y coordinates"
  ([node]
   
   ;; note that sort-by is stable, nodes with equal z preserve their ordering
   (sort-by :z (leaf-nodes node 0 0 0 [])))

  ([node parent-x parent-y parent-z leaves]

   (let [x (+ parent-x (or (:x node)
                           0))
         y (+ parent-y (or (:y node)
                           0))
         z (+ parent-z (or (:z node)
                           0))]
     (if (:children node)
       (loop [leaves leaves
              children (:children node)]
         (if-let [child (first children)]
           (recur (leaf-nodes child x y z leaves)
                  (rest children))
           leaves))
       (conj leaves
             (assoc node
                    :x x
                    :y y
                    :z z))))))

(spec/fdef leaf-nodes
           :args (spec/or :one (spec/cat :node ::node)
                          :two (spec/cat :node ::node
                                         :parent-x ::coordinate
                                         :parent-y ::coordinate
                                         :parent-z ::coordinate
                                         :leaves (spec/coll-of ::node))))

(test/deftest leaf-nodes-test
  (is (= '({:x 10, :y 15, :expected-position 1, :z 0}
           {:x 5, :y 10, :expected-position 2, :z 0}
           {:x 5, :y 10, :expected-position 3, :z 0}
           {:x 15, :y 20, :expected-position 4, :z 10}
           {:x 15, :y 20, :expected-position 5, :z 10})
         (leaf-nodes {:y 5 :x 0 :children [{:x 5 :y 5 :children [{:x 5 :y 5 :expected-position 1}
                                                                 {:x 5 :y 5 :z 10 :children [{:x 5 :y 5 :expected-position 4}
                                                                                             {:x 5 :y 5 :expected-position 5}]}]}
                                           {:x 5 :y 5 :expected-position 2}
                                           {:x 5 :y 5 :expected-position 3}]}))))

(defn flatten
  ([node]
   (cache/call! flatten node 0 0 0 []))

  ([node parent-x parent-y parent-z nodes]
   (let [x (+ parent-x (:x node))
         y (+ parent-y (:y node))
         z (+ parent-z (or (:z node)
                           0))
         nodes (conj nodes
                     (-> (assoc node
                                :x x
                                :y y
                                :z z)
                         (dissoc :children)))]
     (if (:children node)
       (loop [nodes nodes
              children (:children node)]
         (if-let [child (first children)]
           (recur (cache/call! flatten child x y z nodes)
                  (rest children))
           nodes))
       nodes))))


(test/deftest flatten-test
  (is (= [{:y 5, :x 0, :id 1, :z 0}
          {:x 5, :y 10, :id 2, :z 0}
          {:x 10, :y 15, :expected-position 1, :z 0}
          {:x 10, :y 15, :z 10, :id 3}
          {:x 15, :y 20, :expected-position 4, :z 10}
          {:x 15, :y 20, :expected-position 5, :z 10}
          {:x 5, :y 10, :expected-position 2, :z 0}
          {:x 5, :y 10, :expected-position 3, :z 0}]
         (flatten {:y 5 :x 0 :id 1 :children [{:x 5 :y 5 :id 2 :children [{:x 5 :y 5 :expected-position 1}
                                                                          {:x 5 :y 5 :z 10 :id 3 :children [{:x 5 :y 5 :expected-position 4}
                                                                                                            {:x 5 :y 5 :expected-position 5}]}]}
                                              {:x 5 :y 5 :expected-position 2}
                                              {:x 5 :y 5 :expected-position 3}]}))))


(defn conditionaly-flatten 
  ([node descent-predicate include-predicate]
   (conditionaly-flatten node 0 0 0 [] descent-predicate include-predicate))

  ([node parent-x parent-y parent-z nodes descent-predicate include-predicate]
   (let [node-x (+ parent-x (or (:x node)
                                0))
         node-y (+ parent-y (or (:y node)
                                0))
         node-z (+ parent-z (or (:z node)
                                0))
         nodes (let [node (-> (assoc node
                                     :x node-x
                                     :y node-y
                                     :z node-z)
                              (dissoc :children))]
                 (if (or (not include-predicate)
                         (include-predicate node))
                   (conj nodes node)
                   nodes))]
     (if (and (:children node)
              (or (not descent-predicate)
                  (descent-predicate node)))
       (loop [nodes nodes
              children (:children node)]
         (if-let [child (first children)]
           (recur (conditionaly-flatten child node-x node-y node-z nodes descent-predicate include-predicate)
                  (rest children))
           nodes))
       nodes))))

(defn in-coordinates? [node x y]
  (and (>= x
           (:x node))
       (<= x
           (+ (:x node) (:width node)))
       (>= y
           (:y node))
       (<= y
           (+ (:y node) (:height node)))))

(spec/fdef in-coordinates?
           :args (spec/cat :node ::layouted-node
                           :x ::coordinate
                           :y ::coordinate))

(defn hits? [node x y]
  (and (in-coordinates? node x y)
       (if-let [hit-test (:hit-test node)]
         (hit-test node
                   (- x (:x node))
                   (- y (:y node)))
         true)))

(defn intersects? [node-1 node-2]
  (not (or (< (+ (:x node-1)
                 (:width node-1))
              (:x node-2))
           (< (+ (:x node-2)
                 (:width node-2))
              (:x node-1))
           
           (< (+ (:y node-1)
                 (:height node-1))
              (:y node-2))
           (< (+ (:y node-2)
                 (:height node-2))
              (:y node-1)))))

(deftest intersects?-test
  (is (intersects? {:x 0 :y 0 :width 100 :height 100}
                   {:x 0 :y 0 :width 100 :height 100}))

  (is (intersects? {:x 0 :y 0 :width 100 :height 100}
                   {:x 10 :y 10 :width 100 :height 100}))

  (is (intersects? {:x 0 :y 0 :width 100 :height 100}
                   {:x -10 :y 10 :width 200 :height 100}))
  
  (is (not (intersects? {:x 0 :y 0 :width 100 :height 100}
                        {:x -100 :y 10 :width 10 :height 10}))))

(defn update-depth-first [scene-graph predicate function]
  (if-let [children (:children scene-graph)]
    (let [scene-graph (update-in scene-graph
                                 [:children]
                                 (fn [children]
                                   (doall (map (fn [child]
                                                 (update-depth-first child predicate function))
                                               children))))]
      (if (predicate scene-graph)
        (function scene-graph)
        scene-graph))
    (if (predicate scene-graph)
      (do (println (:id scene-graph))
          (function scene-graph)) 
      scene-graph)))

(deftest apply-depth-first-test
  (is (= '{:children
           ({:children
             ({:id 1, :apply true, :applied 1}
              {:id 2, :apply true, :applied 2}),
             :apply true,
             :id 5,
             :applied 3}
            {:id 3}
            {:id 4}),
           :apply true,
           :id 6,
           :applied 4}
         (update-depth-first {:children [{:children [{:id 1
                                                      :apply true}
                                                     {:id 2
                                                      :apply true}]
                                          :apply true
                                          :id 5}
                                         {:id 3}
                                         {:id 4}]
                              :apply true
                              :id 6}
                             
                             :apply
                             
                             (let [count (atom 0)]
                               (fn [node]
                                 (swap! count inc)
                                 (assoc node :applied @count)))))))

(defn bounding-box [nodes]
  {:x1 (apply min (map :x nodes))
   :y1 (apply min (map :y nodes))
   :x2 (apply max (map :x nodes))
   :y2 (apply max (map :y nodes))})

(defn intersection [rectangle-1 rectangle-2]
  (if (or
       ;; 2 is left of 1
       (< (+ (:x rectangle-2)
             (:width rectangle-2))
          (:x rectangle-1))

       (< (+ (:x rectangle-1)
             (:width rectangle-1))
          (:x rectangle-2)))
    nil
    {}))


(deftest intersection-test
  ;; 2 is left of 1
  (is (= nil
         (intersection {:x 0 :y 0 :width 100}
                       {:x -100 :y 0 :width 10})))

  ;; 2 is right of 1
  (is (= nil
         (intersection {:x 0 :y 0 :width 100}
                       {:x 200 :y 0 :width 10}))))

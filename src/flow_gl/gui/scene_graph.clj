(ns flow-gl.gui.scene-graph
  (:require [clojure.spec.alpha :as spec]
            [clojure.test :as test :refer [deftest is]]
            [fungl.cache :as cache]
            [medley.core :as medley]))

(def ^:dynamic current-scene-graph)


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
                         #_(dissoc :children)))]
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
         (map #(dissoc % :children)
              (flatten {:y 5 :x 0 :id 1 :children [{:x 5 :y 5 :id 2 :children [{:x 5 :y 5 :expected-position 1}
                                                                               {:x 5 :y 5 :z 10 :id 3 :children [{:x 5 :y 5 :expected-position 4}
                                                                                                                 {:x 5 :y 5 :expected-position 5}]}]}
                                                   {:x 5 :y 5 :expected-position 2}
                                                   {:x 5 :y 5 :expected-position 3}]})))))


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


(defn enumerate-nodes [node]
  (loop [nodes-left [node]
         nodes []]
    (if-let [node (first nodes-left)]
      (recur (concat (rest nodes-left) (:children node))
             (conj nodes node))
      nodes)))


(test/deftest flatten-test
  (is (= '({:id 1} {:id 2} {:id 3} {:id 4})
         (map #(dissoc % :children)
              (enumerate-nodes {:children [{:children [{:id 3}
                                                       {:id 4}]
                                            :id 2}]
                                :id 1})))))

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

(defn in-region? [x y width height node]
  (intersects? {:x 0 :y 0 :width width :height height}
               node))

(defn update-depth-first [scene-graph predicate function]
  (let [scene-graph (if-let [children (:children scene-graph)]
                      (update-in scene-graph
                                 [:children]
                                 (fn [children]
                                   (doall (map (fn [child]
                                                 (update-depth-first child predicate function))
                                               children))))
                      scene-graph)]
    (if (predicate scene-graph)
      (let [result (function scene-graph)]
        (spec/assert ::node result)
        result)
      scene-graph)))

(deftest update-depth-first-test
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

(defn find-first [predicate scene-graph]
  (if (predicate scene-graph)
    scene-graph
    (loop [children (:children scene-graph)]
      (when-let [child (first children)]
        (or (find-first predicate child)
            (recur (rest children)))))))

(deftest test-find-first
  (is (= {:id 1}
         (find-first #(= 1 (:id %))
                     {:children [{:children [{:id 1}
                                             {:id 2}]
                                  :id 5}
                                 {:id 3}
                                 {:id 4}]
                      :id 6})))

  (is (= nil
         (find-first (constantly false)
                     {:children [{:children [{:id 1}
                                             {:id 2}]
                                  :id 5}
                                 {:id 3}
                                 {:id 4}]
                      :id 6})))

  (is (= {:children [{:children [{:id 1}
                                 {:id 2}]
                      :id 5}
                     {:id 3}
                     {:id 4}]
          :id 6}
         (find-first #(= 6 (:id %))
                     {:children [{:children [{:id 1}
                                             {:id 2}]
                                  :id 5}
                                 {:id 3}
                                 {:id 4}]
                      :id 6}))))


(defn find-first-child [predicate scene-graph]
  (loop [children (:children scene-graph)]
    (when-let [child (first children)]
      (or (find-first predicate child)
          (recur (rest children))))))

(deftest test-find-first-child
  (is (= {:id 1}
         (find-first-child #(= 1 (:id %))
                           {:children [{:id 1}]
                            :id 1}))))

(defn find-first-child-breath-first [predicate scene-graph]
  (or (some (fn [child]
              (when (predicate child)
                child))
            (:children scene-graph))
      (some (partial find-first-child-breath-first predicate)
            (:children scene-graph))))

(defn find-first-breath-first [predicate scene-graph]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY
                     scene-graph)]
    (if (empty? queue)
      nil
      (let [node (peek queue)]
        (if (predicate node)
          node
          (recur (into (pop queue)
                       (:children node))))))))

(deftest test-find-first-breath-first
  (is (= {:id 1}
         (find-first-breath-first #(= 1 (:id %))
                                  {:children [{:children [{:id 1}
                                                          {:id 2}]
                                               :id 5}
                                              {:id 3}
                                              {:id 4}]
                                   :id 6})))

  (is (= nil
         (find-first-breath-first (constantly false)
                                  {:children [{:children [{:id 1}
                                                          {:id 2}]
                                               :id 5}
                                              {:id 3}
                                              {:id 4}]
                                   :id 6})))

  (is (= {:id 4, :type :x}
         (find-first-breath-first #(= :x (:type %))
                                  {:children [{:children [{:id 1
                                                           :type :x}
                                                          {:id 2}]
                                               :id 3}
                                              {:id 4
                                               :type :x}]
                                   :id 5})))

  (is (= {:id 2, :type :x}
         (find-first-breath-first #(= :x (:type %))
                                  {:children [{:children [{:children [{:id 1
                                                                       :type :x}]}]}
                                              {:children [{:id 2
                                                           :type :x}]}]})))

  (is (= {:id 2, :type :x}
         (find-first-breath-first #(= :x (:type %))
                                  {:children [{:children [{:id 2
                                                           :type :x}]}
                                              {:children [{:children [{:id 1
                                                                       :type :x}]}]}]}))))

(defn- path-to-first* [path predicate scene-graph]
  (if (predicate scene-graph)
    (conj path scene-graph)
    (loop [children (:children scene-graph)]
      (when-let [child (first children)]
        (or (path-to-first* (conj path scene-graph) predicate child)
            (recur (rest children)))))))

(defn path-to-first [predicate scene-graph]
  (path-to-first* [] predicate scene-graph))

(deftest test-path-to-first
  (is (= [{:children [{:children [{:id 1} {:id 2}], :id 5} {:id 3} {:id 4}],
           :id 6}
          {:children [{:id 1} {:id 2}], :id 5}
          {:id 2}]
         (path-to-first (comp #{2} :id)
                        {:children [{:children [{:id 1}
                                                {:id 2}]
                                     :id 5}
                                    {:id 3}
                                    {:id 4}]
                         :id 6}))))

(defn path-to [scene-graph node-id]
  (path-to-first (comp #{node-id}
                       :id)
                 scene-graph))

(defn id-to-local-id-path [id]
  (vec (remove #{:call} id)))

(deftest test-id-to-local-id-path
  (is (= [0 1]
         (id-to-local-id-path [0 :call 1])))

  (is (= []
         (id-to-local-id-path nil))))

(defn nodes-on-local-id-path [root-node local-id-path]
  (loop [nodes [root-node]
         local-ids local-id-path]
    (if-let [local-id (first local-ids)]
      (let [parent (last nodes)]
        (if-let [child (if (number? local-id)
                         (when (and (>= local-id 0)
                                    (< local-id
                                       (count (:children parent))))
                           (nth (:children parent)
                                local-id))
                         (medley/find-first #(= local-id (:local-id %))
                                            (:children parent)))]
          (recur (conj nodes child)
                 (rest local-ids))
          nodes))
      nodes)))

(deftest test-nodes-on-local-id-path
  (is (= [{:id []}]
         (nodes-on-local-id-path {:id []}
                                 [])))

  (is (= [{:id []}]
         (nodes-on-local-id-path {:id []}
                                 [0])))

  (is (= [{:id []
           :children [{:type :child}]}
          {:type :child}]
         (nodes-on-local-id-path {:id []
                                  :children [{:type :child}]}
                                 [0])))

  (is (= [{:id []
           :children [{:type :child}]}]
         (nodes-on-local-id-path {:id []
                                  :children [{:type :child}]}
                                 [10])))

  (is (= [{:id [],
           :children
           [{:local-id :a
             :children [{:id [:a 0]}]}
            {:local-id :b
             :children [{:id [:b 0]}]}]}
          {:local-id :b, :children [{:id [:b 0]}]}
          {:id [:b 0]}]
         (nodes-on-local-id-path {:id []
                                  :children [{:local-id :a
                                              :children [{:id [:a 0]}]}
                                             {:local-id :b
                                              :children [{:id [:b 0]}]}]}
                                 [:b 0]))))

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


(defn above? [below above]
  (> (:y below)
     (:y above)))

(defn horizontal-distance [node-1 node-2]
  (Math/abs (- (:x node-1)
               (:x node-2))))

(defn next-above [reference-node nodes]
  (first (->> nodes
              (filter (partial above? reference-node))
              (sort-by (fn [node]
                         [(- (:y reference-node))
                          (horizontal-distance reference-node node)]))
              (reverse))))

(defn below? [above below]
  (> (:y below)
     (:y above)))

(defn next-below [reference-node nodes]
  (first (->> nodes
              (filter (partial below? reference-node))
              (sort-by (fn [node]
                         [(:y reference-node)
                          (horizontal-distance reference-node node)])))))

(defn map-nodes [function scene-graph & [{:keys [descend?] :or {descend? (constantly true)}}]]
  (let [result (function scene-graph)]
    (if-let [children (:children scene-graph)]
      (if (descend? scene-graph)
        (assoc result
               :children (map (partial map-nodes function) children))
        result)
      result)))


(def left-edge :x)

(def top-edge :y)

(defn right-edge [node]
  (+ (:x node)
     (:width node)))

(defn bottom-edge [node]
  (+ (:y node)
     (:height node)))

(defn horizontal-center [node]
  (+ (:x node)
     (/ (:width node)
        2)))

(defn vertical-center [node]
  (+ (:y node)
     (/ (:height node)
        2)))

(defn select-closest-on-one-half-dimension [group-function measure reference-node nodes]
  (->> nodes
       (group-by group-function)
       (medley/map-vals (fn [nodes]
                          (first (sort-by (fn [node]
                                            (Math/abs (- (measure node)
                                                         (measure reference-node))))
                                          nodes))))
       (vals)
       (sort-by group-function)))

(defn closest-nodes-on-one-dimension [reference-node-id minimum maximum orthogonal-center nodes]
  (let [reference-node (medley/find-first #(= reference-node-id (:id %))
                                          nodes)]
    (concat (select-closest-on-one-half-dimension maximum
                                                  orthogonal-center
                                                  reference-node
                                                  (filter (fn [node]
                                                            (<= (maximum node)
                                                                (minimum reference-node)))
                                                          nodes))
            [reference-node]
            (select-closest-on-one-half-dimension minimum
                                                  orthogonal-center
                                                  reference-node
                                                  (filter (fn [node]
                                                            (>= (minimum node)
                                                                (maximum reference-node)))
                                                          nodes)))))

(defn closest-horizontal-nodes [reference-node-id nodes]
  (closest-nodes-on-one-dimension reference-node-id
                                  left-edge
                                  right-edge
                                  vertical-center
                                  nodes))

(defn closest-vertical-nodes [reference-node-id nodes]
  (closest-nodes-on-one-dimension reference-node-id
                                  top-edge
                                  bottom-edge
                                  horizontal-center
                                  nodes))

(defn bounding-box [nodes]
  (let [min-x (apply min (map :x nodes))
        min-y (apply min (map :y nodes))]
    {:x min-x
     :y min-y
     :width (- (apply max (map #(+ (:x %)
                                   (:width %))
                               nodes))
               min-x)
     :height (- (apply max (map #(+ (:y %)
                                    (:height %))
                                nodes))
                min-y)}))

(defn select-node-keys [keys scene-graph]
  (map-nodes #(select-keys % keys)
             scene-graph))

(defn print-scene-graph
  "use with select-node-keys"
  ([scene-graph]
   (print-scene-graph 0 scene-graph))

  ([level scene-graph]
   (println (str (apply str (repeat (* level 2) " "))
                 (pr-str (dissoc scene-graph
                                 :children))))

   (run! (partial print-scene-graph (inc level))
         (:children scene-graph))))

(defn leaf-nodes-in-view [width height leaf-nodes]
  (filter (fn [node]
            (intersects? {:x 0 :y 0 :width width :height height}
                         node))
          leaf-nodes))

(defn nodes-in-view [scene-graph width height]
  (leaf-nodes-in-view width height
                      (cache/call! leaf-nodes scene-graph)))

(defn transpose [x y node]
  (-> node
      (update :x #(+ % x))
      (update :y #(+ % y))))

(defn clip [clipped clipper]
  (assoc clipped
         :x (max (:x clipped)
                 (:x clipper))
         :y (max (:y clipped)
                 (:y clipper))
         :width (min (:width clipped)
                     (max 0
                          (- (+ (:x clipper)
                                (:width clipper))
                             (:x clipped))))

         :height (min (:height clipped)
                      (max 0
                           (- (+ (:y clipper)
                                 (:height clipper))
                              (:y clipped))))))

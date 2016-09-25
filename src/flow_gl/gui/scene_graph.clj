(ns flow-gl.gui.scene-graph
  (:require [clojure.spec :as spec]
            [clojure.test :as test :refer [deftest is]]))

#_(defprotocol Node
    (children [this]))

#_(extend Object
    Node {::children (fn [this] (::children this))})

(spec/def ::coordinate int?)
(spec/def ::x ::coordinate)
(spec/def ::y ::coordinate)
(spec/def ::z ::coordinate)
(spec/def ::width int?)
(spec/def ::height int?)
(spec/def ::children (spec/* ::node))

(spec/def ::node (spec/keys :req-un [::x ::y]
                            :opt-un [::z ::children]))

(spec/def ::layouted-children (spec/* ::layouted-node))
(spec/def ::layouted-node (spec/merge ::node
                                      (spec/keys :req-un [::width ::height]
                                                 :opt-un [::layouted-children])))


#_(spec/explain ::node {::x 1 ::y 1})

(defn leave-nodes
  "Returns leave nodes in depth first and ascending z order with global x and y coordinates"
  ([node]
   
   ;; note that sort-by is stable, nodes with equal z preserve their ordering
   (sort-by :z (leave-nodes node 0 0 0 [])))

  ([node parent-x parent-y parent-z leaves]

   (let [x (+ parent-x (:x node))
         y (+ parent-y (:y node))
         z (+ parent-z (or (:z node)
                           0))]
     (if (:children node)
       (loop [leaves leaves
              children (:children node)]
         (if-let [child (first children)]
           (recur (leave-nodes child x y z leaves)
                  (rest children))
           leaves))
       (conj leaves
             (assoc node
                    :x x
                    :y y
                    :z z))))))

(test/deftest leave-nodes-test
  (is (= '({:x 10, :y 15, :expected-position 1, :z 0}
           {:x 5, :y 10, :expected-position 2, :z 0}
           {:x 5, :y 10, :expected-position 3, :z 0}
           {:x 15, :y 20, :expected-position 4, :z 10}
           {:x 15, :y 20, :expected-position 5, :z 10})
         (leave-nodes {:y 5 :x 0 :children [{:x 5 :y 5 :children [{:x 5 :y 5 :expected-position 1}
                                                                  {:x 5 :y 5 :z 10 :children [{:x 5 :y 5 :expected-position 4}
                                                                                              {:x 5 :y 5 :expected-position 5}]}]}
                                            {:x 5 :y 5 :expected-position 2}
                                            {:x 5 :y 5 :expected-position 3}]}))))

(defn flatten
  ([node]
   (flatten node 0 0 0 []))

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
           (recur (flatten child x y z nodes)
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


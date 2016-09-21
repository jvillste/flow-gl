(ns flow-gl.gui.scene-graph
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as spec-test]
            [clojure.test :as test :refer [deftest is]]))

#_(defprotocol Node
    (children [this]))

#_(extend Object
    Node {::children (fn [this] (::children this))})

(spec/def ::coordinate int?)
(spec/def ::x ::coordinate)
(spec/def ::y ::coordinate)
(spec/def ::z ::coordinate)
(spec/def ::children (spec/* ::node))
(spec/def ::node (spec/keys :req [::x ::y]
                            :opt [::z ::children]))

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
           (let [leaves (leave-nodes child x y z leaves)]
             (recur leaves
                    (rest children)))
           leaves))
       (conj leaves
             (assoc node
                    :x x
                    :y y
                    :z z))))))

#_(spec/fdef leave-nodes
             :args (spec/cat :node ::node
                             :rest (spec/* (constantly true))))

#_(spec-test/instrument (spec-test/instrumentable-syms))

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



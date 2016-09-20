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



(defn leaves-with-global-coordinates
  ([node]
   (leaves-with-global-coordinates node 0 0 [] []))

  ([node parent-x parent-y parent-z leaves]

   (let [x (+ parent-x (::x node))
         y (+ parent-y (::y node))
         z (conj parent-z (or (::z node) 0))]
     (if (::children node)
       (loop [leaves leaves
              children (::children node)]
         (if-let [child (first children)]
           (let [leaves (leaves-with-global-coordinates child x y z leaves)]
             (recur leaves
                    (rest children)))
           leaves))
       (conj leaves
             (assoc node
                    ::x x
                    ::y y
                    ::z z))))))

(spec/fdef leaves-with-global-coordinates
           :args (spec/cat :node ::node
                           :rest (spec/* (constantly true))))

(spec-test/instrument (spec-test/instrumentable-syms))

(test/deftest leaves-with-global-coordinates-test
  (is (= [{::x 15, ::y 10, ::z [0 1]}
          {::x 20, ::y 15, ::z [0 0]}]
         (leaves-with-global-coordinates {::y 5 ::children [{::x 5 ::y 5 ::z 1}
                                                                   {::x 10 ::y 10}]}))))



(ns fungl.layout.placing
  (:require
   clojure.data
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
   [fungl.view-compiler :as view-compiler]))

(defn uncached-set-child-size [child width height]
  (assoc child
         :width width
         :height height))

(defn set-child-size [child width height]
  (hierarchical-identity-cache/call-with-cache view-compiler/compile-node-cache-atom
                                               (:compilation-path child)
                                               1
                                               uncached-set-child-size
                                               child
                                               width
                                               height))

(defn uncached-place-child [child x y width height]
  (let [unplaced-child (set-child-size child
                                       width
                                       height)]
    (assoc unplaced-child
           :x x
           :y y
           :unplaced-node unplaced-child)))

(defn place-child [child x y width height]
  (hierarchical-identity-cache/call-with-cache view-compiler/compile-node-cache-atom
                                               (:compilation-path child)
                                               1
                                               uncached-place-child
                                               child
                                               x
                                               y
                                               width
                                               height))

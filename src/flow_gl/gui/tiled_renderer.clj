(ns flow-gl.gui.tiled-renderer
  (:require (flow-gl.opengl.jogl [render-target :as render-target])
            (flow-gl.gui [stateful :as stateful]
                         [render-target-renderer :as render-target-renderer])
            (fungl [renderer :as renderer]))
  (:use clojure.test))

(defn initialize-state [gl]
  {:tiles {}})

(defn tiles-in-view [tile-size x y width height]
  (for [x (range (int (/ x
                         tile-size))
                 (int (/ (+ x width)
                         tile-size)))
        y (range (int (/ y
                         tile-size))
                 (int (/ (+ y height)
                         tile-size)))]
    [x y]))

(deftest tiles-in-view-test
  (is (= '([0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2])
         (tiles-in-view 100 10 10 300 300)))
  
  (is (= '([1 1] [1 2] [2 1] [2 2])
         (tiles-in-view 100 110 110 250 250))))

(defn render-to-tile [tile scene-graph]
  tile)

(defn create-tile [size coordinates scene-graph]
  {:source-scene-graph scene-graph
   :coordinates coordinates
   :size size})


(defn render [renderers state-atom gl scene-graph]
  (let [tile-size 300
        tile-coordinates (tiles-in-view tile-size
                                        0 0
                                        (:width scene-graph) (:height scene-graph))
        previous-tiles (:previous-tiles @state-atom)
        tiles (for [coordinates tile-coordinates]
                (if-let [tile (get previous-tiles
                                   coordinates)]
                  (if (= scene-graph
                         (:source-scene-graph tile))
                    tile
                    (render-to-tile tile))
                  (create-tile coordinates
                               scene-graph)))]
    (assoc (select-keys scene-graph [:x :y :width :height])
           :children (map :result-scene-graph tiles))))


(defn renderer [renderers]
  {:initialize-state initialize-state
   :render (partial render renderers)
   
   :delete-state (fn [state])})


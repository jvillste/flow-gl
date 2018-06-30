(ns flow-gl.gui.tiled-renderer
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [flow-gl.graphics.font :as font]
            [flow-gl.gui.render-target-renderer :as render-target-renderer]
            [flow-gl.gui.visuals :as visuals]
            [fungl.renderer :as renderer]
            [taoensso.timbre.profiling :as timbre-profiling]))

(defn initialize-state [gl]
  {:tiles {}})

(defn window-range [window-width offset length]
  (if (< 0 window-width)
    (range (int (Math/floor (/ offset
                               window-width)))
           (int (Math/ceil (/ (+ offset length)
                              window-width))))
    []))

(defn tiles-in-view [tile-width tile-height view-x view-y view-width view-height]
  (for [view-x (window-range tile-width view-x view-width)
        view-y (window-range tile-height view-y view-height)]
    [view-x view-y]))

(deftest tiles-in-view-test
  (is (= '([0 0] [0 1] [1 0] [1 1])
         (tiles-in-view 100 100 10 10 100 100)))
  
  (is (= '([1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2] [3 3])
         (tiles-in-view 100 100 110 110 250 250)))

  (is (= '([0 0])
         (tiles-in-view 100 100 0 0 50 50)))

  (is (= '([-1 -1])
         (tiles-in-view 100 100 -20 -20 10 10))))

(def font (font/create (.getPath (io/resource "LiberationSans-Regular.ttf")) 15))


(defn render-tile [tile tiled-renderer-id renderers tile-width tile-height visualize-tiles scene-graph gl]
  (let [stripped-scene-graph (dissoc scene-graph :renderers :x :y)]
    #_(prn (first (data/diff stripped-scene-graph
                             (:source-scene-graph tile))))
    (assoc tile
           :width tile-width
           :height tile-height
           :source-scene-graph stripped-scene-graph
           :result-scene-graph (let [[x y] (:coordinates tile)]
                                 (-> (if (and (= stripped-scene-graph
                                                 (:source-scene-graph tile))
                                              (= (:width tile)
                                                 tile-width)
                                              (= (:height tile)
                                                 tile-height))
                                       (:result-scene-graph tile)
                                       (let [pixel-x (* x tile-width)
                                             pixel-y (* y tile-height)]
                                         (timbre-profiling/p :render-tile
                                                             (renderer/apply-renderers! {:children (cond-> [(assoc stripped-scene-graph
                                                                                                                   :x (- pixel-x)
                                                                                                                   :y (- pixel-y))]
                                                                                                     visualize-tiles
                                                                                                     (concat
                                                                                                      [(assoc (visuals/rectangle [255 255 255 50] 0 0)
                                                                                                              :x 1
                                                                                                              :y 1
                                                                                                              :width (- tile-width 1)
                                                                                                              :height (- tile-height 1))
                                                                                                       (assoc (visuals/rectangle [0 0 0 155] 15 15)
                                                                                                              :x 3
                                                                                                              :y 3
                                                                                                              :width 60
                                                                                                              :height 20)
                                                                                                       (assoc (visuals/text [255 0 0 255] font
                                                                                                                            (str x "," y))
                                                                                                              :x 10
                                                                                                              :y 5)]))
                                                                                         :x pixel-x
                                                                                         :y pixel-y
                                                                                         :width tile-width
                                                                                         :height tile-height
                                                                                         :renderers [(assoc (apply render-target-renderer/renderer
                                                                                                                   renderers)
                                                                                                            :id [tiled-renderer-id ::tile x y])]} 
                                                                                        gl)))))))))

(defn create-tile [coordinates]
  {:coordinates coordinates})

(defn render [renderers tile-width tile-height tile-coordinates visualize-tiles state-atom gl scene-graph]
  (let [tiles (reduce (fn [tiles coordinates]
                        (assoc tiles
                               coordinates
                               (-> (or (get tiles
                                            coordinates)
                                       (create-tile coordinates))
                                   (render-tile (:flow-gl.gui.stateful/id @state-atom)
                                                renderers
                                                tile-width
                                                tile-height
                                                visualize-tiles
                                                scene-graph
                                                gl))))
                      (:previous-tiles @state-atom)
                      tile-coordinates)]
    (swap! state-atom assoc :previous-tiles tiles)
    (assoc (select-keys scene-graph [:x :y :width :height])
           :children (map :result-scene-graph
                          (vals (select-keys tiles tile-coordinates)))))
  #_(timbre-profiling/profile :info :render-tiled
                              ))

(def stateful {:initialize-state initialize-state})

(defn renderer [renderers tile-width tile-height coordinates & {:keys [visualize-tiles] :or {visualize-tiles false}}]
  {:initialize-state initialize-state
   :render (partial render renderers tile-width tile-height coordinates visualize-tiles)
   
   ;;:delete-state (fn [state])
   })




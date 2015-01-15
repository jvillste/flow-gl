(ns flow-gl.gui.transformer
  (:require
   (flow-gl.gui [renderer :as renderer]
                [drawable :as drawable])

   (flow-gl.opengl.jogl [opengl :as opengl]
                        [render-target :as render-target]
                        [quad :as quad]
                        [shader :as shader]))
  (:import [nanovg NanoVG]
           [flow_gl.gui.drawable Quad]
           [javax.media.opengl GL2])
  (:use clojure.test))

(defprotocol Transformer
  (transform [this gpu-state drawables x y width height]))

(defn transpose [node x y z]
  (assoc node
    :x (+ x (:x node))
    :y (+ y (:y node))
    :z (+ z (or (:z node) 0))))

(def render-trees-for-layout)

(defn child-render-trees [layoutable]
  (loop [trees []
         children (:children layoutable)]
    (if-let [child (first children)]
      (recur (render-trees-for-layout child (:x layoutable) (:y layoutable) (or (:z layoutable) 0) trees)
             (rest children))
      trees)))


(defn render-trees-for-layout
  ([layoutable]
     (render-trees-for-layout layoutable 0 0 0 []))

  ([layoutable parent-x parent-y parent-z trees]
     (if (:transformers layoutable)
       (let [transformer-node (-> layoutable
                                  (select-keys [:x :y :z :width :height :transformers :children])
                                  (transpose parent-x
                                             parent-y
                                             parent-z))]
         (conj trees
               (if (:children layoutable)
                 (assoc transformer-node
                   :children (child-render-trees transformer-node))
                 layoutable)))

       (if (:children layoutable)
         (concat trees
                 (child-render-trees (transpose layoutable
                                                parent-x
                                                parent-y
                                                parent-z)))
         (conj trees
               (transpose layoutable
                          parent-x
                          parent-y
                          parent-z))))))

(deftest render-trees-for-layout-test
  (is (= '({:y 10, :z 0, :id 8, :x 20}
           {:children
            [{:y 10, :transformers [:highlight-3], :id 3, :x 0}
             {:z 0, :y 20, :id 4, :x 30}],
            :transformers [:highlight-1],
            :z 0,
            :y 10,
            :x 30}
           {:y 0, :transformers [:highlight-2], :id 6, :x 0}
           {:z 0, :y 20, :id 7, :x 0})
         (render-trees-for-layout {:x 20 :y 10 :id 1
                                   :children [
                                              {:x 10 :y 0 :z 0
                                               :id 2
                                               :transformers [:highlight-1]
                                               :children [{:x 0 :y 10 :id 3 :transformers [:highlight-3]}
                                                          {:x 0 :y 10 :id 4}]}
                                              {:x 0 :y 10 :id 5
                                               :children [{:x 0 :y 0 :id 6 :transformers [:highlight-2]}
                                                          {:x 0 :y 10 :id 7}]}
                                              {:x 0 :y 0 :z 0
                                               :id 8}]}))))



(defn with-transformers [& transformers-and-layoutable]
  (assoc (last transformers-and-layoutable) :transformers (drop-last transformers-and-layoutable)))

(defn apply-transformers [gpu-state render-tree]
  (let [[gpu-state child-drawables] (if (:children render-tree)
                                      (loop [gpu-state  gpu-state
                                             drawables []
                                             children (:children render-tree)]
                                        (if-let [child-tree (first children)]
                                          (let [[gpu-state  child-drawables] (apply-transformers gpu-state child-tree (:gl gpu-state))]
                                            (recur gpu-state
                                                   (concat drawables child-drawables)
                                                   (rest children)))
                                          [gpu-state
                                           drawables]))
                                      [gpu-state
                                       [render-tree]])]


    (loop [gpu-state  gpu-state
           drawables child-drawables
           transformers (:transformers render-tree)]
      (if-let [transformer (first transformers)]
        (let [[gpu-state drawables] (transform transformer
                                               gpu-state
                                               drawables
                                               (:x render-tree)
                                               (:y render-tree)
                                               (:width render-tree)
                                               (:height render-tree))]
          (recur gpu-state
                 drawables
                 (rest transformers)))
        [gpu-state
         drawables]))))

(defn transform-trees [gpu-state render-trees]
  (loop [render-trees render-trees
         gpu-state gpu-state
         all-drawables []]
    (if-let [render-tree (first render-trees)]
      (let [[gpu-state drawables] (apply-transformers gpu-state
                                                      render-tree)]
        (recur (rest render-trees)
               gpu-state
               (concat all-drawables drawables)))
      [gpu-state all-drawables])))


;; Transformers

(defrecord Highlight [key]
  Transformer
  (transform [this gpu-state drawables x y width height]
    [gpu-state
     (concat drawables
             (map (fn [drawable]
                    (assoc (drawable/->Rectangle (:width drawable) (:height drawable) [255 0 0 155])
                      :x (:x drawable) :y (:y drawable) :z 1 ))
                  (filter :highlight? drawables)))]))

(defrecord HighlightAll [key color]
  Transformer
  (transform [this gpu-state drawables x y width height]
    [gpu-state
     (concat drawables
             (map (fn [drawable]
                    (assoc (drawable/->Rectangle (:width drawable) (:height drawable) color)
                      :x (:x drawable) :y (:y drawable) :z 1 ))
                  drawables))]))

(defrecord FilterState [renderers render-target]
  TransformerState
  (dispose [this gl]
    (doseq [renderer renderers]
      (renderer/delete renderer gl))
    (when render-target
      (render-target/delete render-target gl))))

(defrecord Filter [key fragment-shader-source uniforms]
  Transformer
  (transform [this gpu-state drawables x y width height]
    (let [gl (:gl gpu-state)
          drawables (map (fn [drawable]
                           (assoc drawable
                             :y (- (:y drawable) y)
                             :x (- (:x drawable) x)))
                         drawables)
          old-render-target (get-in gpu-state [:key :render-target])
          render-target (if (and old-render-target
                                 (= (:width old-render-target)
                                    width)
                                 (= (:height old-render-target)
                                    height))
                          old-render-target
                          (render-target/create width
                                                height
                                                gl))]

      (when (and old-render-target
                 (not= old-render-target render-target))
        (render-target/delete old-render-target gl))

      (let [renderers (render-target/render-to render-target gl
                                               ;;(opengl/initialize-gl gl)
                                               (opengl/clear gl 0 0 0 0)
                                               (renderer/render-frame-drawables drawables
                                                                                gl
                                                                                (:renderers gpu-state)))]

        [(-> gpu-state
             (assoc :renderers renderers)
             (assoc-in [:key :render-target] render-target))
         [(drawable/->Quad ["texture" (:texture render-target)]
                           uniforms
                           fragment-shader-source
                           x y
                           width
                           height)]]))))


(run-all-tests)

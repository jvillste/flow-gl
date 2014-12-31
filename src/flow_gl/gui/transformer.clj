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

(defprotocol StatelessTransformer
  (transform [this drawables x y width height gl]))

(defprotocol StatefulTransformer
  (transform-with-state [this state drawables x y width height gl])
  (initialize-state [this gl]))

(defprotocol TransformerState
  (dispose [this gl]))


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

(defn apply-transformers [transformer-states render-tree gl]
  (let [[transformer-states child-drawables] (if (:children render-tree)
                                               (loop [transformer-states transformer-states
                                                      drawables []
                                                      children (:children render-tree)]
                                                 (if-let [child-tree (first children)]
                                                   (let [[transformer-states child-drawables] (apply-transformers transformer-states child-tree gl)]
                                                     (recur transformer-states
                                                            (concat drawables child-drawables)
                                                            (rest children)))
                                                   [transformer-states
                                                    drawables]))
                                               [transformer-states
                                                [render-tree]])]


    (loop [transformer-states transformer-states
           drawables child-drawables
           transformers (:transformers render-tree)]
      (if-let [transformer (first transformers)]

        (if (satisfies? StatefulTransformer transformer)
          (let [transformer-state (or (get-in transformer-states [:states (:key transformer)] )
                                      (initialize-state transformer gl))
                [transformer-state drawables] (transform-with-state transformer
                                                                    transformer-state
                                                                    drawables
                                                                    (:x render-tree)
                                                                    (:y render-tree)
                                                                    (:width render-tree)
                                                                    (:height render-tree)
                                                                    gl)]
            (recur (-> transformer-states
                       (assoc-in [:states (:key transformer)] transformer-state)
                       (update-in [:used-state-keys] conj (:key transformer)))
                   drawables
                   (rest transformers)))
          (recur transformer-states
                 (transform transformer
                            drawables
                            (:x render-tree)
                            (:y render-tree)
                            (:width render-tree)
                            (:height render-tree)
                            gl)
                 (rest transformers)))
        [transformer-states
         drawables]))))

(defn transform-trees [transformer-states render-trees gl]
  (let [transformer-states (assoc transformer-states :used-state-keys #{})
        [transformer-states drawables] (loop [render-trees render-trees
                                              transformer-states transformer-states
                                              all-drawables []]
                                         (if-let [render-tree (first render-trees)]
                                           (let [[transformer-states drawables] (apply-transformers transformer-states
                                                                                                    render-tree
                                                                                                    gl)]
                                             (recur (rest render-trees)
                                                    transformer-states
                                                    (concat all-drawables drawables)))
                                           [transformer-states all-drawables]))
        unused-transformer-state-keys (filter (complement (:used-state-keys transformer-states))
                                              (keys (:states transformer-states)))]

    (dorun (map (fn [unused-transformer-state-key]
                  (dispose (get-in transformer-states [:states unused-transformer-state-key])
                           gl))
                unused-transformer-state-keys))

    [(update-in transformer-states [:states] dissoc unused-transformer-state-keys)
     drawables]))


;; Transformers

(defrecord Highlight [key]
  StatelessTransformer
  (transform [this drawables x y width height gl]
    (concat drawables
            (map (fn [drawable]
                   (assoc (drawable/->Rectangle (:width drawable) (:height drawable) [255 0 0 155])
                     :x (:x drawable) :y (:y drawable) :z 1 ))
                 (filter :highlight? drawables)))))

(defrecord FilterState [renderers render-target]
  TransformerState
  (dispose [this gl]
    (doseq [renderer renderers]
      (renderer/delete renderer gl))
    (when render-target
      (render-target/delete render-target gl))))

(defrecord Filter [key fragment-shader-source uniforms]
  StatefulTransformer
  (transform-with-state [this state drawables x y width height gl]
    (let [drawables (map (fn [drawable]
                           (assoc drawable
                             :y (- (:y drawable) y)
                             :x (- (:x drawable) x)))
                         drawables)
          old-render-target (:render-target state)
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
                                                                                (:renderers state)))]

        [(assoc state
           :renderers renderers
           :render-target render-target)
         [(drawable/->Quad ["texture" (:texture render-target)]
                           uniforms
                           fragment-shader-source
                           x y
                           width
                           height)]])))

  (initialize-state [this gl]
    (->FilterState [(renderer/create-quad-view-renderer gl)
                    (renderer/create-nanovg-renderer)
                    (renderer/create-quad-renderer gl)]
                   nil)))


(defrecord CacheState [renderers render-target previous-drawables]
  TransformerState
  (dispose [this gl]
    (doseq [renderer renderers]
      (renderer/delete renderer gl))
    (when render-target
      (render-target/delete render-target gl))))

(defrecord Cache [key]
  StatefulTransformer
  (transform-with-state [this state drawables x y width height gl]
    (if (and (:render-target state)
             (= width (-> state :render-target :width))
             (= height (-> state :render-target :height))
             (= #_identical? drawables (:previous-drawables state)))
      (do (flow-gl.debug/add-event :cache-transformer-hit)
          [state
           [(drawable/->Quad ["texture" (:texture (:render-target state))]
                             []
                             quad/fragment-shader-source
                             x y
                             width
                             height)]])
      
      (let [drawables (map (fn [drawable]
                           (assoc drawable
                             :y (- (:y drawable) y)
                             :x (- (:x drawable) x)))
                         drawables)
          old-render-target (:render-target state)
          render-target (if (and old-render-target
                                 (= (:width old-render-target)
                                    width)
                                 (= (:height old-render-target)
                                    height))
                          old-render-target
                          (render-target/create width
                                                height
                                                gl))]
        (flow-gl.debug/add-event :cache-transformer-miss)

      (when (and old-render-target
                 (not= old-render-target render-target))
        (render-target/delete old-render-target gl))

      (let [renderers (render-target/render-to render-target gl
                                               ;;(opengl/initialize-gl gl)
                                               (opengl/clear gl 0 0 0 0)
                                               (renderer/render-frame-drawables drawables
                                                                                gl
                                                                                (:renderers state)))]

        [(assoc state
           :previous-drawables drawables
           :renderers renderers
           :render-target render-target)
         [(drawable/->Quad ["texture" (:texture render-target)]
                           []
                           quad/fragment-shader-source
                           x y
                           width
                           height)]]))))

  (initialize-state [this gl]
    (->CacheState [(renderer/create-quad-view-renderer gl)
                   (renderer/create-nanovg-renderer)
                   (renderer/create-quad-renderer gl)]
                  nil
                  nil)))


(defrecord RenderTransformerState [renderers]
  TransformerState
  (dispose [this gl]
    (doseq [renderer renderers]
      (renderer/delete renderer gl))))

(defrecord RenderTransformer [key]
  StatefulTransformer
  (transform-with-state [this state drawables x y width height gl]
    [(assoc state
       :renderers (renderer/render-frame drawables
                                         gl
                                         (:renderers state)))
     []])

  (initialize-state [this gl]
    (->RenderTransformerState [(renderer/create-quad-view-renderer gl)
                               (renderer/create-nanovg-renderer)
                               (renderer/create-quad-renderer gl)])))


(run-all-tests)

(ns examples.render-targets-with-renderers
  (:require [clojure.data.priority-map :as priority-map]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]
                                 [quad :as quad])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layoutable :as layoutable]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [renderer :as renderer]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 1]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))


(defn text [text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 30)
                   [1 1 1 1]))



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
                 (child-render-trees layoutable))
         (conj trees
               (transpose layoutable
                          parent-x
                          parent-y
                          parent-z))))))

(deftest render-trees-for-layout-test
  (is (= '({:children
            [{:y 10, :transformers [:highlight-3], :id 3, :x 0}
             {:z 0, :y 20, :id 4, :x 30}],
            :transformers [:highlight-1],
            :z 0,
            :y 10,
            :x 30}
           {:y 0, :transformers [:highlight-2], :id 6, :x 0}
           {:z 0, :y 20, :id 7, :x 0})
         (render-trees-for-layout {:x 20 :y 10 :id 1
                                   :children [{:x 10 :y 0 :z 0
                                               :id 2
                                               :transformers [:highlight-1]
                                               :children [{:x 0 :y 10 :id 3 :transformers [:highlight-3]}
                                                          {:x 0 :y 10 :id 4}]}
                                              {:x 0 :y 10 :id 5
                                               :children [{:x 0 :y 0 :id 6 :transformers [:highlight-2]}
                                                          {:x 0 :y 10 :id 7}]}]}))))

(defprotocol StatelessTransformer
  (transform [this drawables x y width height gl]))

(defprotocol StatefulTransformer
  (transform-with-state [this state drawables x y width height gl])
  (initialize-state [this gl]))

(defprotocol TransformerState
  (dispose [this gl]))

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
    (println "running" key drawables)
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

(defn set-transformers [layoutable & transformers]
  (assoc layoutable :transformers transformers))

(defn with-transformers [& transformers-and-layoutable]
  (assoc (last transformers-and-layoutable) :transformers (drop-last transformers-and-layoutable)))

(defn render-trees-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]
    (render-trees-for-layout
     (let [[state layout] (layout/layout (-> (with-transformers
                                               (->Highlight :highlight)
                                               (->Filter :fade1
                                                           quad/alpha-fragment-shader-source
                                                           [:1f "alpha" 0.7])
                                               (layouts/->VerticalStack
                                                [(text "child 1")
                                                 (assoc (text "child 2") :highlight? true)
                                                 (-> (with-transformers (->Filter :fade2
                                                                                  quad/alpha-fragment-shader-source
                                                                                  [:1f "alpha" 0.7])
                                                       (text "child 3")))]))

                                             (assoc :width 200
                                                    :height 200
                                                    :x 0
                                                    :y 0))

                                         {}
                                         200 200)]
       layout))))

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

(defn transform-tree [transformer-states render-tree gl]

  (let [[transformer-states drawables] (apply-transformers (assoc transformer-states :used-state-keys #{})
                                                           render-tree
                                                           gl)
        unused-transformer-state-keys (filter (complement (:used-state-keys transformer-states))
                                              (keys (:states transformer-states)))]
    (dorun (map (fn [unused-transformer-state-key]
                  (dispose (get-in transformer-states [:states unused-transformer-state-key])
                           gl))
                unused-transformer-state-keys))

    [(update-in transformer-states [:states] dissoc unused-transformer-state-keys)
     drawables]))

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

(defn start-view []
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)
        render-tree-state-atom (atom nil)]

    (try
      (loop []
        (let [frame-started (System/currentTimeMillis)
              render-trees (render-trees-for-time frame-started)]
          (flow-gl.debug/ppreturn render-trees)
          (window/set-display window gl
                              (opengl/clear gl 0 0 0 1)
                              (let [{:keys [width height]} (opengl/size gl)]
                                (swap! render-tree-state-atom
                                       (fn [render-tree-state]
                                         (let [[render-tree-state drawables] (transform-tree render-tree-state
                                                                                             {:transformers [(->RenderTransformer :root)]
                                                                                              :children render-trees
                                                                                              :width width
                                                                                              :height height
                                                                                              :x 0
                                                                                              :y 0}
                                                                                             gl)]
                                           render-tree-state)))))

          (when (window/visible? window)
            (do (wait-for-next-frame frame-started)
                (recur)))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view))

(run-tests)

#_( transform drawables to multiple textures with filtering and transposing)

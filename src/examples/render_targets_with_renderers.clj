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
                                  (select-keys [:x :y :z :transformers :children])
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
  (initialize-state [this gl])
  (dispose-state [this state gl]))

(defrecord Highlight [key]
  StatelessTransformer
  (transform [this drawables x y width height gl]
    (concat drawables
            (map (fn [drawable]
                   (assoc (drawable/->Rectangle (:width drawable) (:height drawable) [255 0 0 155])
                     :x (:x drawable) :y (:y drawable) :z 1 ))
                 (filter :highlight? drawables)))))

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
    {:renderers [(renderer/create-quad-view-renderer gl)
                 #_(renderer/create-nanovg-renderer)]})

  (dispose-state [this state gl]
    (dorun (map renderer/delete (:renderers this)))
    (render-target/delete (:render-target this) gl)))

(defn set-transformers [layoutable & transformers]
  (assoc layoutable :transformers transformers))

(defn render-trees-for-time [time]
  (let [phase (/ (mod time 1000)
                 1000)]
    (gui/drawables-for-layout
     (let [[state layout] (layout/layout (-> (layouts/->VerticalStack
                                              [(layouts/->Margin 0 0 0 10 [(text "child 1")])
                                               (assoc (text "child 2") :highlight? true)
                                               #_(-> (text "child 3")
                                                     (set-transformers (->Filter :fade
                                                                                 quad/alpha-fragment-shader-source
                                                                                 [:1f "alpha" 0.2])))])
                                             (set-transformers (->Highlight :highlight))
                                             (assoc
                                                 :width 200
                                                 :height 200
                                                 :x 0
                                                 :y 0))

                                         {}
                                         200 200)]
       #_(flow-gl.debug/ppreturn layout)
       layout))))

#_(->> {:transformers [{:key :transformer-1}
                       {:key :transformer-2}]
        :children [{:id :drawable-1}] #_{}}
       :children
       (filter :transformers)
       (mapcat :transformers)
       (map :key)
       (apply hash-set))

(defn transform-tree [render-tree-state render-tree gl]

  (let [old-child-render-target-state-keys (->> render-tree-state
                                                :child-render-tree-states
                                                keys
                                                (apply hash-set))
        new-child-render-target-state-keys (->> render-tree
                                                :children
                                                (filter :transformers)
                                                (mapcat :transformers)
                                                (map :key)
                                                (apply hash-set))
        [render-tree-state child-drawables] (reduce (fn [[render-tree-state child-drawables] drawable]
                                                      (if (:render-target? drawable)
                                                        (let [child-state-path [:child-render-tree-states (:key drawable)]
                                                              child-render-target-state (or (get-in render-tree-state child-state-path)
                                                                                            (if (:stateful? drawable)
                                                                                              ((:constructor drawable) gl)
                                                                                              {}))
                                                              [child-render-target-state child-render-target-drawables] (transform-tree child-render-target-state
                                                                                                                                        drawable
                                                                                                                                        gl)]
                                                          [(assoc-in render-tree-state child-state-path child-render-target-state)
                                                           (concat child-drawables child-render-target-drawables)])


                                                        [render-tree-state
                                                         (conj child-drawables drawable)]))
                                                    [render-tree-state []]
                                                    (:children render-tree ))]

    (dorun (map (fn [child-render-target-key]
                  (let [child-render-target-state (get-in render-tree-state [:child-render-tree-states child-render-target-key])]
                    (when-let [destructor (:destructor child-render-target-state)]
                      (destructor
                       child-render-target-state
                       gl))))
                (filter (complement new-child-render-target-state-keys)
                        old-child-render-target-state-keys)))

    (if (:stateful? render-tree )
      ((:render render-tree )
       render-tree-state
       child-drawables
       (:x render-tree )
       (:y render-tree )
       (:width render-tree )
       (:height render-tree )
       gl)

      [render-tree-state
       ((:render render-tree )
        child-drawables
        (:x render-tree )
        (:y render-tree )
        (:width render-tree )
        (:height render-tree )
        gl)])))

(defrecord RenderTransformer [key]
  StatefulTransformer
  (transform-with-state [this state drawables x y width height gl]
    [(assoc state
       :renderers (renderer/render-frame drawables
                                         gl
                                         (:renderers state)))
     []])

  (initialize-state [this gl]
    {:renderers [(renderer/create-quad-view-renderer gl)
                 (renderer/create-nanovg-renderer)
                 (renderer/create-quad-renderer gl)]})

  (dispose-state [this state gl]
    (dorun (map renderer/delete (:renderers state)))))

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

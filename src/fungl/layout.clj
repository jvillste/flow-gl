(ns fungl.layout
  (:require
   clojure.data
   [clojure.spec.alpha :as spec]
   [flow-gl.gui.scene-graph :as scene-graph]
   [fungl.callable :as callable]
   [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]
   [fungl.layout.measuring :as measuring]
   [fungl.layout.placing :as placing]
   [fungl.view-compiler :as view-compiler]))

(def ^:dynamic layout-node-cache-atom)
(def ^:dynamic adapt-to-space-cache-atom)


(defn state-bindings []
  {#'layout-node-cache-atom (hierarchical-identity-cache/create-cache-atom)
   #'adapt-to-space-cache-atom (hierarchical-identity-cache/create-cache-atom)})

(spec/def ::available-width int?)
(spec/def ::available-height int?)
(spec/def ::node-with-space (spec/keys :req-un [::available-width ::available-height]))

(defn adapt-to-space [node available-width available-height]
  (if-some [adapt-to-space-callable (:adapt-to-space node)]
    (adapt-to-space (->> (callable/call-with-hierarchical-identity-cache view-compiler/compile-node-cache-atom
                                                                         (:compilation-path node)
                                                                         0
                                                                         adapt-to-space-callable
                                                                         node
                                                                         available-width
                                                                         available-height)
                         (view-compiler/call-compile-node-with-cache (:id node)
                                                                     (conj (:compilation-path node)
                                                                           :adapt-to-space)))
                    available-width available-height)
    node))

(defn log-node [message node]
  (println message (:id node) (System/identityHashCode node))
  node)

(declare cached-measure)

(defn measure [node available-width available-height]
  (-> node
      (adapt-to-space available-width available-height)
      (cond->
          (some? (:children node))
        (update :children
                (fn [children]
                  (mapv (fn [child available-area]
                          (cached-measure child
                                          (:available-width available-area)
                                          (:available-height available-area)))
                        children
                        (if-some [available-area-for-children (:available-area-for-children node)]
                          (available-area-for-children node
                                                       available-width
                                                       available-height)
                          (repeat (count children)
                                  {:available-width available-width
                                   :available-height available-height}))))))
      (measuring/add-size available-width available-height)))

(defn cached-measure [node available-width available-height]
  (hierarchical-identity-cache/call-with-cache view-compiler/compile-node-cache-atom
                                               (:compilation-path node)
                                               1
                                               measure
                                               node
                                               available-width
                                               available-height))

(declare cached-make-layout)

(defn make-layout [node]
  (-> node
      ;;      (save-layout)
      (measuring/make-layout)
      (cond->
          (some? (:children node))
        (update :children
                (fn [children]
                  (mapv cached-make-layout
                        children))))))

(defn cached-make-layout [node]
  (hierarchical-identity-cache/call-with-cache view-compiler/compile-node-cache-atom
                                               (:compilation-path node)
                                               1
                                               make-layout
                                               node))

(defn layout-node-in-two-passes [node available-width available-height]
  (-> node
      (cached-measure available-width available-height)
      (cached-make-layout)))

(defn- layout-root [scene-graph available-width available-height]
  (placing/place-child (layout-node-in-two-passes scene-graph available-width available-height)
                       0
                       0
                       available-width
                       available-height))

(defn layout-scene-graph [scene-graph available-width available-height]
  (hierarchical-identity-cache/with-cache-cleanup layout-node-cache-atom
    (hierarchical-identity-cache/with-cache-cleanup adapt-to-space-cache-atom
      (let [layouted-scene-graph (hierarchical-identity-cache/call-with-cache view-compiler/compile-node-cache-atom
                                                                              (:compilation-path scene-graph)
                                                                              1
                                                                              layout-root
                                                                              scene-graph
                                                                              available-width
                                                                              available-height)]
        ;; (prn)
        ;; (prn (hierarchical-identity-cache/statistics layout-node-cache-atom))
        ;; (prn (hierarchical-identity-cache/statistics adapt-to-space-cache-atom))
        layouted-scene-graph))))

(defn layouted [create-scene-graph]
  (fn [width height]
    (-> (create-scene-graph)
        (layout-scene-graph width height))))

(def layout-keys [:type :local-id :id :x :y :width :height :available-width :available-height :children :view-call? :can-gain-focus? #_:keyboard-event-handler
                  ])

(defn select-layout-keys [scene-graph]
  (scene-graph/map-nodes #(select-keys % layout-keys)
                         scene-graph))

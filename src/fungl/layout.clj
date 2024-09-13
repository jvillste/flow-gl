(ns fungl.layout
  (:require [clojure.spec.alpha :as spec]
            [fungl.cache :as cache]
            [fungl.callable :as callable]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.view-compiler :as view-compiler]
            [fungl.layout.measuring :as measuring]
            [flow-gl.gui.visuals :as visuals]
            [clojure.test :refer [deftest is]]
            [clj-async-profiler.core :as prof]
            [fungl.hierarchical-identity-cache :as hierarchical-identity-cache]))

(def ^:dynamic layout-cache-atom)

(defn state-bindings []
  {#'layout-cache-atom (hierarchical-identity-cache/create-cache-atom)})

(spec/def ::available-width int?)
(spec/def ::available-height int?)
(spec/def ::node-with-space (spec/keys :req-un [::available-width ::available-height]))

;; TODO keep track of the nodes which layout comes from the cache and use that to cleanup compnents from view-compiler state
;; the view compiler state must be cleaned of removed components only after layout

;; it is enough to collect all cached node ids and then mark all their children as applied view calls when cleaning view call cache

;; this applis to layout cache as well as view compilers scene-graph-cache

;; when a layout comes from the cache, it means the component in that branch of the scene graph have not changed, thus no component cleanup is needed.

;; applied component cache
(defn adapt-to-space [node available-width available-height]
  #_(prn 'adapt-to-space (:id node)) ;; TODO: remove me

  (if-let [callable (:adapt-to-space node)]
    (adapt-to-space (->> (callable/call-with-hierarchical-identity-cache layout-cache-atom
                                                                         (:id node)
                                                                         0
                                                                         callable
                                                                         node
                                                                         available-width
                                                                         available-height)
                         (view-compiler/compile-node (:view-call? node)
                                                     (:id node)
                                                     #_(concat (:id node)
                                                               [:adapted])))
                    available-width available-height)
    node))


;; TODO: would it be possible to adjust child size after children are given sizes? Some children are not given sizes and their sizes might depend on the ones that are given. For example horizontal lines in a table header

(defn save-property [node key]
  (let [storage-key (keyword (str "given-" (name key)))]
    (assoc node
           storage-key
           (if (contains? node storage-key)
             (storage-key node)
             (key node)))))

(deftest test-save-property
  (is (= {:x 1, :given-x 1}
         (save-property {:x 1} :x)))

  (is (= {:x 1 :given-y nil}
         (save-property {:x 1} :y)))

  (is (= {:x 1, :given-x 2}
         (save-property {:x 1
                         :given-x 2} :x))))

(defn save-layout [node]
  (reduce save-property
          node
          [:width :height :x :y]))

(deftest test-save-layout
  (is (= {:x 10,
          :width 20,
          :given-width 20,
          :given-height nil,
          :given-x 10,
          :given-y nil}
         (save-layout {:x 10 :width 20}))))

(defn log-node [message node]
  (println message (:id node) (System/identityHashCode node))
  node)

(declare do-layout
         layout-node)

(defn do-layout-for-children [node available-width available-height]
  (update node
          :children
          (fn [children]
            (if children
              (mapv (fn [child available-area]
                      #_(layout-node child
                                     (:available-width available-area)
                                     (:available-height available-area))
                      (hierarchical-identity-cache/call-with-cache layout-cache-atom
                                                                   (:id child)
                                                                   1
                                                                   layout-node
                                                                   child
                                                                   (:available-width available-area)
                                                                   (:available-height available-area)))
                    children
                    (if-some [available-area-for-children (:available-area-for-children node)]
                      (available-area-for-children node
                                                   available-width
                                                   available-height)
                      (repeat (count children)
                              {:available-width available-width
                               :available-height available-height})))
              nil))))

;; (def count-atom (atom 0))

(defn layout-node [node available-width available-height]
  ;;   (swap! count-atom inc)
  (-> node
      (adapt-to-space available-width available-height)
      (do-layout-for-children available-width available-height)
      (save-layout)
      (measuring/add-size available-width available-height)
      (measuring/make-layout)))

(defn layout-scene-graph [scene-graph available-width available-height]
  (hierarchical-identity-cache/with-cache-cleanup layout-cache-atom
    (let [layouted-scene-graph (assoc (layout-node scene-graph available-width available-height)
                                      :x 0
                                      :y 0
                                      :width available-width
                                      :height available-height)]
      ;; (prn (hierarchical-identity-cache/statistics layout-cache-atom))
      layouted-scene-graph)))

(defn layouted [create-scene-graph]
  (fn [width height]
    (-> (create-scene-graph)
        (layout-scene-graph width height))))

(def layout-keys [:type :local-id :id :x :y :width :height :available-width :available-height :children :view-call? :can-gain-focus? #_:keyboard-event-handler
                  ])

(defn select-layout-keys [scene-graph]
  (scene-graph/map-nodes #(select-keys % layout-keys)
                         scene-graph))

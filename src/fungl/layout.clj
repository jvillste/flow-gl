(ns fungl.layout
  (:require [clojure.spec.alpha :as spec]
            [fungl.cache :as cache]
            [fungl.callable :as callable]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.view-compiler :as view-compiler]
            [fungl.layout.measuring :as measuring]
            [flow-gl.gui.visuals :as visuals]
            [clojure.test :refer [deftest is]]))

;; (def ^:dynamic layout-cache)

;; (defn state-bindings []
;;   {#'layout-cache (cache/create-state 30)})

(spec/def ::available-width int?)
(spec/def ::available-height int?)
(spec/def ::node-with-space (spec/keys :req-un [::available-width ::available-height]))

;; TODO keep track of the nodes which layout comes from the cache and use that to cleanup compnents from view-compiler state
;; the view compiler state must be cleaned of removed components only after layout

;; it is enough to collect all cached node ids and then mark all their children as applied view calls when cleaning view call cache
;; this applis to layout cache as well as view compilers scene-graph-cache

;; when a layout comes from the cache, it means the component in that branch of the scene graph have not changed, thus no component cleanup is needed.

;; applied component cache

(defn adapt-to-space [node]
  #_(prn 'adapt-to-space (:id node)) ;; TODO: remove me

  (if-let [callable (:adapt-to-space node)]
    (let [adapted-node (merge (->> (callable/call-with-cache callable node)
                                   (view-compiler/compile-node (:view-call? node)
                                                               (:id node) #_(concat (:id node)
                                                                                    [:adapted])))
                              (select-keys node [:x :y :available-width :available-height :width :height :view-call?]))]
      (if (:adapt-to-space adapted-node)
        (adapt-to-space adapted-node)
        adapted-node))
    node))

(declare do-layout)


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

(defn- do-layout-implementation [node]
  #_(prn 'do-layout-implementation (:id node)) ;; TODO: remove me

  (-> node
      (save-layout)
      (adapt-to-space)
      (measuring/give-space)
      (update-in [:children]
                 (fn [children]
                   (if children
                     (map do-layout
                          children)
                     nil)))
      (measuring/add-size)
      (measuring/make-layout)))

(defn do-layout [node]
  (cache/call! do-layout-implementation
               node)

  #_(when (and (cache/cached? do-layout-implementation
                              node)
               (view-compiler/invalidated? (:id node)))
      (cache/invalidate! (cache/function-call-key do-layout-implementation
                                                  [node])))

  #_(if (cache/cached? do-layout-implementation
                       node)
      (do #_(println "layout cache hit" (pr-str (:id node)))
          (swap! view-compiler/state
                 update
                 :cached-view-call-ids
                 conj
                 (:id node))
          (let [scene-graph (cache/call! do-layout-implementation
                                         node)]
            (if (:render scene-graph)
              scene-graph
              (assoc scene-graph
                     :render visuals/render-to-images-render-function
                     :render-on-descend? true)))
          #_(cond-> (cache/call! do-layout-implementation
                                 node)
              (:view-call? node)
              (assoc :render visuals/render-to-images-render-function
                     :render-on-descend? true)))
      (cache/call! do-layout-implementation
                   node))

  )

(defn do-layout-for-size [scene-graph window-width window-height]
  (-> scene-graph
      (assoc :available-width window-width
             :available-height window-height)
      (do-layout)))

(defn layouted [create-scene-graph]
  (fn [width height]
    (-> (create-scene-graph)
        (do-layout-for-size width height))))


(def layout-keys [:type :local-id :id :x :y :width :height :available-width :available-height :children :view-call? :can-gain-focus? #_:keyboard-event-handler
                  ])

(defn select-layout-keys [scene-graph]
  (scene-graph/map-nodes #(select-keys % layout-keys)
                         scene-graph))

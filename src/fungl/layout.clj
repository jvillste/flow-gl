(ns fungl.layout
  (:require [clojure.spec.alpha :as spec]
            [fungl.cache :as cache]
            [fungl.callable :as callable]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.view-compiler :as view-compiler]
            [fungl.layout.measuring :as measuring]
            [flow-gl.gui.visuals :as visuals]))

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
    (let [adapted-node (merge (->> (callable/call callable node)
                                   (view-compiler/compile-node (:view-call? node)
                                                               (:id node) #_(concat (:id node)
                                                                                    [:adapted])))
                              (select-keys node [:x :y :available-width :available-height :width :height :view-call?]))]
      (if (:adapt-to-space adapted-node)
        (adapt-to-space adapted-node)
        adapted-node))
    node))

(declare do-layout)

(defn- do-layout-implementation [node]
  #_(prn 'do-layout-implementation (:id node)) ;; TODO: remove me

  (-> node
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

  #_(do-layout-implementation node)

  ;; (when (and (cache/cached? do-layout-implementation
  ;;                           node)
  ;;            (view-compiler/invalidated? (:id node)))
  ;;   (cache/invalidate! (cache/function-call-key do-layout-implementation
  ;;                                               [node])))

  (if (cache/cached? do-layout-implementation
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
      (assoc :x 0
             :y 0
             :available-width window-width
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

(ns fungl.layout
  (:require [clojure.spec.alpha :as spec]
            [fungl.cache :as cache]
            [fungl.callable :as callable]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.view-compiler :as view-compiler]
            [fungl.layout.measuring :as measuring]))

(spec/def ::available-width int?)
(spec/def ::available-height int?)
(spec/def ::node-with-space (spec/keys :req-un [::available-width ::available-height]))

(cache/defn-memoized adapt-to-space [node]
  (if-let [callable (:adapt-to-space node)]
    (->> (callable/call callable node)
         (view-compiler/compile-node []
                                     (:id node)))
    node))

(cache/defn-memoized do-layout [node]
  #_(taoensso.tufte/p :do-layout-called)
  #_(println "do-layout")
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


(def layout-keys [:type :local-id :id :x :y :width :height :available-width :available-height])

(defn select-layout-keys [scene-graph]
  (scene-graph/map-nodes #(select-keys % layout-keys)
                         scene-graph))

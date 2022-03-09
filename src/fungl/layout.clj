(ns fungl.layout
  (:require [clojure.spec.alpha :as spec]
            [fungl.cache :as cache]

            [fungl.callable :as callable]))

(spec/def ::available-width int?)
(spec/def ::available-height int?)
(spec/def ::node-with-space (spec/keys :req-un [::available-width ::available-height]))

(cache/defn-memoized adapt-to-space [node]
  (if-let [callable (:adapt-to-space node)]
    (callable/call callable node)
    node))

(cache/defn-memoized ensure-available-space [node]
  (assoc node
         :available-width (or (:available-width node)
                              java.lang.Integer/MAX_VALUE)
         :available-height (or (:available-height node)
                               java.lang.Integer/MAX_VALUE)))


(cache/defn-memoized give-space [node]
  (if (:children node)
    (let [node (ensure-available-space node)]
      (if-let [give-space-function (:give-space node)]
        (give-space-function node)
        (update-in node [:children]
                   (fn [children]
                     (map (fn [child]
                            (assoc child
                                   :available-width (:available-width node)
                                   :available-height (:available-height node)))
                          children)))))

    node))

(cache/defn-memoized size [node]
  (if-let [get-size (:get-size node)]
    (get-size node)
    {:width (or (:width node)
                (:available-width node))
     :height (or (:height node)
                 (:available-height node))}))

(cache/defn-memoized add-size [node]
  (conj node (size node)))

(cache/defn-memoized make-layout [node]
  (if-let [layout-function (:make-layout node)]
    (layout-function node)
    (update-in node [:children]
               (fn [children]
                 (if children
                   (map (fn [child]
                          (assoc child
                                 :x (or (:x child)
                                        0)
                                 :y (or (:y child)
                                        0)))
                        children)
                   nil)))))

(cache/defn-memoized do-layout [node]
  #_(taoensso.tufte/p :do-layout-called)
  #_(println "do-layout")
  (-> node
      (adapt-to-space)
      (give-space)
      (update-in [:children]
                 (fn [children]
                   (if children
                     (map do-layout
                          children)
                     nil)))
      (add-size)
      (make-layout)))

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
  (let [result (select-keys scene-graph layout-keys)]
    (if-let [children (:children scene-graph)]
      (assoc result
             :children (map select-layout-keys children))
      result)))

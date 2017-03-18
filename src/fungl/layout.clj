(ns fungl.layout
  (:require  [clojure.spec :as spec]
             [fungl.cache :as cache]))

(spec/def ::available-width int?)
(spec/def ::available-height int?)
(spec/def ::node-with-space (spec/keys :req-un [::available-width ::available-height]))

(cache/defn-memoized adapt-to-space [node]
  (if-let [adapt-function (:adapt-to-space node)]
    (adapt-function node)
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

(cache/defn-memoized add-layout [node]
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
  #_(taoensso.timbre.profiling/p :do-layout-called)
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
      (add-layout)))

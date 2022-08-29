(ns fungl.layout.measuring
  (:require [fungl.cache :as cache]
            [fungl.callable :as callable]))

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
    (callable/call get-size node)
    {:width (or (:width node)
                (:available-width node))
     :height (or (:height node)
                 (:available-height node))}))

(cache/defn-memoized add-size [node]
  (conj node (size node)))

(cache/defn-memoized make-layout [node]
  (if-let [make-layout-callable (:make-layout node)]
    (callable/call make-layout-callable node)
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

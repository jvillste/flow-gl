(ns fungl.layout.measuring
  (:require [fungl.cache :as cache]
            [fungl.callable :as callable]))

(defn ensure-available-space [node]
  (assoc node
         :available-width (or (:available-width node)
                              java.lang.Integer/MAX_VALUE)
         :available-height (or (:available-height node)
                               java.lang.Integer/MAX_VALUE)))

(defn give-space [node]
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

(defn size [node available-width available-height]
  (if-let [get-size (:get-size node)]
    (let [size (callable/call get-size node available-width available-height)]
      (assoc size
             :given-width (:width size)
             :given-height (:height size)))
    {:width (or (:given-width node)
                available-width)
     :height (or (:given-height node)
                 available-height)}))

(defn add-size [node available-width available-height]
  (merge node
        (size node available-width available-height)))

(defn make-layout [node]
  (if-let [make-layout-callable (:make-layout node)]
    (as-> node node
      (callable/call make-layout-callable node)
      (update node :children (fn [children]
                               (map (fn [child]
                                      (-> child
                                          (update :x (fn [x]
                                                       (+ x (or (:given-x (:node child))
                                                                0))))
                                          (update :y (fn [y] (+ y (or (:given-y (:node child))
                                                                      0))))))
                                    children))))

    (update-in node [:children]
               (fn [children]
                 (if children
                   (map (fn [child]
                          {:node child
                           :x (or (:given-x child)
                                  0)
                           :y (or (:given-y child)
                                  0)
                           :width (:width child)
                           :height (:height child)})
                        children)
                   nil)))))

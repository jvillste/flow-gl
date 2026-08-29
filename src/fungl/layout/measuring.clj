(ns fungl.layout.measuring
  (:require [fungl.cache :as cache]
            [fungl.layout.placing :as placing]
            [fungl.callable :as callable]))

(defn size [node available-width available-height]
  (if-let [get-size (:get-size node)]
    (callable/call get-size
                   node
                   available-width
                   available-height)
    {:width (or (:width node)
                available-width)
     :height (or (:height node)
                 available-height)}))

(defn add-size [node available-width available-height]
  (merge node
         (size node available-width available-height)))

(defn make-layout [node]
  (if-let [make-layout-callable (:make-layout node)]
    (callable/call make-layout-callable node)
    (if (contains? node :children)
      (update node :children
              (fn [children]
                (map (fn [child]
                       (placing/place-child child
                                            (or (:x child)
                                                0)
                                            (or (:y child)
                                                0)
                                            (:width child)
                                            (:height child)))
                     children)))
      node)))

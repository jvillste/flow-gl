(ns fungl.layouts
  (:require  [clojure.spec :as spec]
             (fungl [cache :as cache]
                    [layout :as layout])
             [flow-gl.gui.scene-graph :as scene-graph]))

;; common

(defn flatten-contents [values]
  (->> values
       (filter (complement nil?))
       (flatten)
       (filter (complement nil?))))


(defn get-first-child-size [node]
  (layout/size (first (:children node))))

;; vertically

(def vertical-stack
  {:get-size (fn [node]
               {:width (apply max
                              (conj (map :width (:children node))
                                    0))
                :height (+ (* (dec (count (:children node)))
                              (:margin node))
                           (reduce + (map :height (:children node))))})
   
   :give-space (fn [node]
                 (update-in node [:children]
                            (fn [children]

                              (map (fn [child]
                                     (assoc child
                                            :available-width (:available-width node)
                                            :available-height java.lang.Integer/MAX_VALUE))
                                   children))))
   :make-layout (fn [node]
                  (assoc node :children
                         (loop [layouted-nodes []
                                y 0
                                children (:children node)]
                           (if-let [child (first children)] 
                             (recur (conj layouted-nodes
                                          (assoc child
                                                 :x 0
                                                 :y y))
                                    (+ y (:height child)
                                       (:margin node))
                                    (rest children))
                             layouted-nodes))))})

(defn vertically [& children]
  (assoc vertical-stack
         :margin 0
         :children (flatten-contents children)))

(defn vertically-with-margin [margin & children]
  (assoc vertical-stack
         :margin margin
         :children (flatten-contents children)))

(def horizontal-stack
  {:get-size (fn [node]
               {:width (+ (* (dec (count (:children node)))
                             (:margin node))
                          (reduce + (map :width (:children node))))
                :height (apply max
                               (conj (map :height (:children node))
                                     0))})
   
   :give-space (fn [node]
                 (update-in node [:children]
                            (fn [children]
                              (map (fn [child]
                                     (assoc child
                                            :available-width java.lang.Integer/MAX_VALUE
                                            :available-height (:available-width node)))
                                   children))))
   :make-layout (fn [node]
                  (assoc node :children
                         (loop [layouted-nodes []
                                x 0
                                children (:children node)]
                           (if-let [child (first children)] 
                             (recur (conj layouted-nodes
                                          (assoc child
                                                 :x x
                                                 :y 0))
                                    (+ x (:width child)
                                       (:margin node))
                                    (rest children))
                             layouted-nodes))))})

(defn horizontally [& children]
  (assoc horizontal-stack
         :margin 0
         :children (flatten-contents children)))

(defn horizontally-with-margin [margin & children]
  (assoc horizontal-stack
         :margin margin
         :children (flatten-contents children)))

;; box

(spec/def ::margin int?)

(defn box-get-size [{:keys [margin children]}]
  (let [[outer inner] children]
    {:width (+ (* 2 margin)
               (:width inner))
     :height (+ (* 2 margin)
                (:height inner))}))

(defn box-give-space [{:keys [available-width available-height margin] :as node}]
  (spec/assert ::margin margin)
  (update-in node [:children]
             (fn [[outer inner]]
               [(assoc outer
                       :available-width available-width
                       :available-height available-height)
                (assoc inner
                       :available-width (- available-width
                                           (* 2 margin))
                       :available-height (- available-height
                                            (* 2 margin)))])))

(defn box-make-layout [{:keys [width height margin] :as node}]
  (update-in node
             [:children]
             (fn [[outer inner]]
               [(assoc outer
                       :x 0
                       :y 0
                       :z 0
                       :width (max width
                                   (+ (:width inner)
                                      (* 2 margin)))
                       :height (max height
                                    (+ (:height inner)
                                       (* 2 margin))))
                (assoc inner
                       :x margin
                       :y margin
                       :z 1)])))

(defn box [margin outer inner]
  (when (and outer inner)
    {:get-size box-get-size
     :give-space box-give-space
     :make-layout box-make-layout
     :margin margin
     :children [outer inner]}))


;; transpose


(defn transpose-make-layout [{:keys [dx dy] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(assoc child
                       :x (+ (or (:x child)
                                 0)
                             dx)
                       :y (+ (or (:y child)
                                 0)
                             dy))])))

(defn transpose [dx dy child]
  (when child
    {:make-layout transpose-make-layout
     :dx dx
     :dy dy
     :children [child]}))

;; scale



(defn scale-node [node x-scale y-scale]
  (-> node
      (update-in [:x] (fnil * 0) x-scale)
      (update-in [:width] * x-scale)
      (update-in [:y] (fnil * 0) y-scale)
      (update-in [:height] * y-scale)))

(defn adapt-node-to-scale [node x-scale y-scale]
  (if-let [adapt-to-scale (:adapt-to-scale node)]
    (adapt-to-scale node x-scale y-scale)
    node))

(defn scale-get-size [node]
  (let [{:keys [x y width height]} (scale-node (first (:children node))
                                               (:x-scale node)
                                               (:y-scale node))]
    {:width (+ x width)
     :height (+ y height)}))

(defn scale-make-layout [{:keys [x-scale y-scale] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(scene-graph/update-depth-first child
                                                identity
                                                (fn [node]
                                                  (-> node
                                                      (scale-node x-scale y-scale)
                                                      (adapt-node-to-scale x-scale y-scale))))])))


(defn scale [x-scale y-scale child]
  (when child
    {:make-layout scale-make-layout
     :get-size scale-get-size
     :x-scale x-scale
     :y-scale y-scale
     :children [child]}))

;; with-minimum-size

(defn get-limited-size [{:keys [width-limit height-limit compare-function children]}]
  {:width (if width-limit
            (compare-function width-limit
                              (:width (first children)))
            (:width (first children)))
   
   :height (if height-limit
             (compare-function height-limit
                               (:height (first children)))
             (:height (first children)))})



(defn make-limited-layout [{:keys [width height] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(layout/make-layout (assoc child
                                           :x 0
                                           :y 0
                                           :z 0
                                           :width width
                                           :height height))])))

(defn give-limited-space [{:keys [width-limit height-limit compare-function available-width available-height children] :as node}]
  (assoc node :children
         [(assoc (first children)
                 :available-width (if width-limit
                                    (compare-function width-limit
                                                      available-width)
                                    available-width) 
                 :available-height (if height-limit
                                     (compare-function height-limit
                                                       available-height)
                                     available-height))]))

(defn with-minimum-size [minimum-width minimum-height child]
  (when child
    {:get-size get-limited-size
     :give-space give-limited-space
     :make-layout make-limited-layout
     :compare-function max
     
     :width-limit minimum-width
     :height-limit minimum-height
     
     :children [child]}))


;; with-maximum-size

(defn with-maximum-size [maximum-width maximum-height child]
  (when child
    {:get-size get-limited-size
     :give-space give-limited-space
     :make-layout make-limited-layout

     :width-limit maximum-width
     :height-limit maximum-height
     :compare-function min
     
     :children [child]}))


;; with-margins

(defn with-margins-get-size [{:keys [left-margin right-margin top-margin bottom-margin children]}]
  (let [child (first children)]
    {:width (+ left-margin
               right-margin
               (:width child))
     :height (+ top-margin
                bottom-margin
                (:height child))}))

(defn with-margins-give-space [{:keys [available-width available-height left-margin right-margin top-margin bottom-margin] :as node}]
  (update-in node [:children]
             (fn [[child]]
               [(assoc child
                       :available-width (- available-width
                                           left-margin
                                           right-margin)
                       :available-height (- available-height
                                            top-margin
                                            bottom-margin))])))

(defn with-margins-make-layout [{:keys [left-margin top-margin] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(assoc child
                       :x left-margin
                       :y top-margin)])))

(defn with-margins [top-margin right-margin bottom-margin left-margin child]
  (when child
    {:get-size with-margins-get-size
     :give-space with-margins-give-space
     :make-layout with-margins-make-layout

     :top-margin top-margin
     :right-margin right-margin
     :bottom-margin bottom-margin
     :left-margin left-margin

     :children [child]}))


;; superimpose

(defn superimpose-get-size [node]
  (let [child-sizes (map layout/size
                         (:children node))]
    {:width (apply max
                   (map :width
                        child-sizes))
     :height (apply max
                    (map :height
                         child-sizes))}))

(defn superimpose [& children]
  (let [children (flatten-contents children)]
    {:children children
     :get-size superimpose-get-size}))

;; preferred-size



(defn preferred-size-make-layout [node]
  (let [child (first (:children node))
        {:keys [width height]} (layout/size node)]
    (assoc node :children [(assoc child
                                  :width width
                                  :height height)])))

(defn with-preferred-size [child]
  {:children [child]
   :get-size get-first-child-size
   :make-layout preferred-size-make-layout})



;; flow

(defn flow-row [nodes maximum-width]
  (loop [height 0
         row-width 0
         row-nodes []
         remaining-nodes nodes]
    (if-let [node (first remaining-nodes)]
      (let [node-size (layout/size node)]
        (if (and (> (+ row-width
                       (:width node-size))
                    maximum-width)
                 (not (empty? row-nodes)))
          {:row-nodes row-nodes
           :height height
           :unused-nodes remaining-nodes}
          (recur (max height (:height node-size))
                 (+ row-width
                    (:width node-size))
                 (conj row-nodes node)
                 (rest remaining-nodes))))
      {:row-nodes row-nodes
       :height height
       :unused-nodes remaining-nodes})))

(defn make-flow-row-layout [nodes y height]
  (loop [x 0
         nodes nodes
         layouted-nodes []]
    (if-let [node (first nodes)]
      (let [preferred-size (layout/size node)]
        (recur (+ x
                  (:width preferred-size))
               (rest nodes)
               (conj layouted-nodes
                     (assoc node
                            :x x
                            :y y
                            :width (:width preferred-size)
                            :height height))))
      layouted-nodes)))

(defn make-flow-layout [node]
  (assoc node :children
         (loop [layouted-children []
                y 0
                children (:children node)]
           (if (seq children)
             (let [row (flow-row children (:available-width node))]
               (recur (concat layouted-children (make-flow-row-layout (:row-nodes row)
                                                                      y
                                                                      (:height row)))
                      (+ y
                         (:height row))
                      (:unused-nodes row)))
             layouted-children))))

(defn get-flow-size [node]
  {:width (:available-width node)
   :height (loop [y 0
                  children (:children node)]
             (if (seq children)
               (let [row (flow-row children (:available-width node))]
                 (recur (+ y
                           (:height row))
                        (:unused-nodes row)))
               y))})


(defn flow [& children]
  {:get-size get-flow-size
   :make-layout make-flow-layout
   :children (flatten-contents children)})

(ns fungl.layouts
  (:require [clojure.spec.alpha :as spec]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.layout.measuring :as measuring]
            [clojure.test :refer :all]
            [medley.core :as medley]))

;; common

(defn- sequential-but-not-vector? [value]
  (and (not (vector? value))
       (sequential? value)))

(defn flatten-except-vectors [value]
  (filter (complement sequential-but-not-vector?)
          (rest (tree-seq sequential-but-not-vector?
                          seq value))))

(deftest test-faltten-except-vectors
  (is (= '(1 2 3 [1 2 3])
         (flatten-except-vectors '((1 2 3)
                                   [1 2 3]))))

  (is (= '(1 2 3 4 5 [1 2 3])
         (flatten-except-vectors '((1 2 (3 4 5))
                                   [1 2 3])))))

(defn flatten-contents [values]
  (if (vector? values)
    values
    (->> values
         (remove nil?)
         (flatten-except-vectors)
         (remove nil?))))

(deftest test-faltten-except-vectors
  (is (= '(1 2 3 [1 2 3])
         (flatten-contents '((1 2 3)
                             [1 2 3]))))

  (is (= '(1 2 3 [1 2 nil 3])
         (flatten-contents '((1 2 nil 3)
                             nil
                             [1 2 nil 3]
                             nil))))

  (is (vector? (flatten-contents [1 2 3]))))

(defn get-first-child-size [node]
  (measuring/size (first (:children node))))

;; vertically

(defn- vertical-stack-get-size
  [node]
  {:width (apply max
                 (conj (map :width (:children node))
                       0))
   :height (if (empty? (:children node))
             0
             (+ (* (dec (count (:children node)))
                   (:margin node))
                (reduce + (map :height (:children node)))))})

(defn- vertical-stack-give-space
  [node]
  (update-in node [:children]
             (fn [children]

               (map (fn [child]
                      (assoc child
                             :available-width (:available-width node)
                             :available-height java.lang.Integer/MAX_VALUE))
                    children))))

(defn- vertical-stack-make-layout
  [node]
  (assoc node :children
         (loop [layouted-nodes []
                y 0
                children (:children node)]
           (if-let [child (first children)]
             (recur (conj layouted-nodes
                          (assoc child
                                 :x (if (::centered node)
                                      (/ (- (:width node)
                                            (:width child))
                                         2)
                                      0)
                                 :y y))
                    (+ y (:height child)
                       (:margin node))
                    (rest children))
             layouted-nodes))))

(def vertical-stack
  {:type ::vertical-stack
   :get-size vertical-stack-get-size
   :give-space vertical-stack-give-space
   :make-layout vertical-stack-make-layout})

(defn vertically [& children]
  (assoc vertical-stack
         :margin 0
         :children (flatten-contents children)))

(defn vertically-2 [options & children]
  (assoc vertical-stack
         :margin (or (:margin options)
                     0)
         ::centered (or (:centered options)
                        false)
         :children (flatten-contents children)))

(defn vertically-with-margin [margin & children]
  (assoc vertical-stack
         :margin margin
         :children (flatten-contents children)))

(defn- horizontal-stack-get-size
  [node]
  {:width (if (empty? (:children node))
            0
            (+ (* (dec (count (:children node)))
                  (:margin node))
               (reduce + (map :width (:children node)))))
   :height (apply max
                  (conj (map :height (:children node))
                        0))})

(defn- horizontal-stack-give-space
  [node]
  (update-in node [:children]
             (fn [children]
               (map (fn [child]
                      (assoc child
                             :available-width java.lang.Integer/MAX_VALUE
                             :available-height (:available-width node)))
                    children))))

(defn- horizontal-stack-make-layout
  [node]
  (assoc node :children
         (loop [layouted-nodes []
                x 0
                children (:children node)]
           (if-let [child (first children)]
             (recur (conj layouted-nodes
                          (assoc child
                                 :x x
                                 :y (if (::centered node)
                                      (/ (- (:height node)
                                            (:height child))
                                         2)
                                      0)))
                    (+ x (:width child)
                       (:margin node))
                    (rest children))
             layouted-nodes))))

(def horizontal-stack
  {:type ::horizontal-stack
   :get-size horizontal-stack-get-size

   ;; TODO: how to give each child only remaining space? is their size known here?
   :give-space horizontal-stack-give-space
   :make-layout horizontal-stack-make-layout})

(defn horizontally [& children]
  (assoc horizontal-stack
         :margin 0
         :children (flatten-contents children)))

(defn horizontally-2 [options & children]
  (assoc horizontal-stack
         :margin (or (:margin options)
                     0)
         ::centered (:centered options)
         :children (flatten-contents children)))

(defn horizontally-with-margin [margin & children]
  (assoc horizontal-stack
         :margin margin
         :children (flatten-contents children)))

;; center

(defn center-make-layout [node]
  (update node :children
          (fn [[child]]
            [(assoc child
                    :x (/ (- (:width node)
                             (:width child))
                          2)
                    :y (/ (- (:height node)
                             (:height child))
                          2))])))

(defn center [child]
  {:make-layout center-make-layout
   :children [child]})

(defn- center-horizontally-make-layout
  [node]
  (update node :children
          (fn [[child]]
            [(assoc child
                    :x (/ (- (:width node)
                             (:width child))
                          2)
                    :y 0)])))

(defn- center-horizontally-get-size
  [node]
  {:width (:available-width node)
   :height (:height (first (:children node)))})

(defn center-horizontally [child]
  {:make-layout center-horizontally-make-layout
   :children [child]
   :get-size center-horizontally-get-size})


(defn center-vertically-make-layout [node]
  (update node :children
          (fn [[child]]
            [(assoc child
                    :x 0
                    :y (/ (- (:height node)
                             (:height child))
                          2))])))

(defn- center-vertically-get-size
  [node]
  {:width (:width (first (:children node)))
   :height (:available-height node)})

(defn center-vertically [child]
  {:make-layout center-vertically-make-layout
   :children [child]
   :get-size center-vertically-get-size})

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
    {:type ::box
     :get-size box-get-size
     :give-space box-give-space
     :make-layout box-make-layout
     :margin margin
     :children [outer inner]}))


;; hover

(defn hover-give-space [node]
  (update node
          :children
          (fn [[child]]
            [(assoc child
                    :available-width java.lang.Integer/MAX_VALUE
                    :available-height java.lang.Integer/MAX_VALUE)])))

(defn- hover-get-size
  [_node]
  {:width 0
   :height 0})

(defn hover-make-layout [options node]
  (update node
          :children
          (fn [[child]]
            [(assoc child
                    :x 0
                    :y 0
                    :z (:z options))])))
(defn hover
  ([child]
   (hover {:z 1} child))

  ([options child]
   {:children [child]
    :get-size hover-get-size

    :give-space hover-give-space

    :make-layout (partial hover-make-layout options)}))

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
               [(measuring/make-layout (assoc child
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
    {:type ::with-minimum-size
     :get-size get-limited-size
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
                       :x (+ (or (:x child)
                                 0)
                             left-margin)
                       :y (+ (or (:y child)
                                 0)
                             top-margin))])))

(defn with-margins [top-margin right-margin bottom-margin left-margin child]
  (when child
    {:type ::with-margins
     :get-size with-margins-get-size
     :give-space with-margins-give-space
     :make-layout with-margins-make-layout

     :top-margin top-margin
     :right-margin right-margin
     :bottom-margin bottom-margin
     :left-margin left-margin

     :children [child]}))


;; superimpose

(defn superimpose-get-size [node]
  (let [child-sizes (map (fn [child]
                           (merge (select-keys child [:x :y])
                                  (measuring/size child)))
                         (:children node))]
    {:width (apply max
                   (map (fn [child-size]
                          (+ (or (:x child-size) 0)
                             (:width child-size)))
                        child-sizes))
     :height (apply max
                    (map (fn [child-size]
                           (+ (or (:y child-size) 0)
                              (:height child-size)))
                         child-sizes))}))

(defn superimpose [& children]
  (let [children (flatten-contents children)]
    (if (empty? children)
      {:type ::superimpose}
      {:type ::superimpose
       :children children
       :get-size superimpose-get-size})))

(defn overlay-get-size [node]
  (measuring/size (first (:children node))))

(defn overlay-make-layout [node]
  (update node
          :children
          (fn [[under over]]
            [(assoc under
                    :x 0
                    :y 0)
             (assoc over
                    :x 0
                    :y 0
                    :z (inc (or (:z under)
                                0))
                    :width (:width under)
                    :height (:height under))])))

(defn overlay [under over]
  {:type ::overlay
   :children [under over]
   :get-size overlay-get-size
   :make-layout overlay-make-layout})


;; preferred-size


(defn preferred-size-make-layout [node]
  (let [child (first (:children node))
        {:keys [width height]} (measuring/size node)]
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
      (let [node-size (measuring/size node)]
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
      (let [preferred-size (measuring/size node)]
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


;; vertical-split

(defn- vertical-split-get-size [node]
  {:width (:available-width node)
   :height (:available-height node)})

(defn- vertical-split-give-space [node]
  (let [row-height (/ (:available-height node)
                            (count (:children node)))]
    (update node
            :children
            (fn [children]
              (->> children
                   (map (fn [child]
                          (assoc child
                                 :available-width (:available-width node)
                                 :available-height row-height))))))))

(defn- vertical-split-make-layout [node]
  (let [row-height (/ (:available-height node)
                      (count (:children node)))]
    (update node :children
            (fn [children]
              (->> children
                   (map-indexed (fn [row-number child]
                                  (assoc child
                                         :x 0
                                         :y (* row-number row-height)))))))))

(defn vertical-split [& children]
  {:type ::vertical-split
   :children (flatten-contents children)
   :get-size vertical-split-get-size
   :give-space vertical-split-give-space
   :make-layout vertical-split-make-layout})


;; grid

(defn- grid-get-size [node]
  (let [row-sizes (->> (partition-by ::row (:children node))
                       (map (fn [row]
                              {:width (reduce + (map :width row))
                               :height (apply max (map :height row))})))]
    {:width (apply max (map :width row-sizes))
     :height (reduce + (map :height row-sizes))}))

(defn- grid-give-space [node]
    (update node
            :children
            (fn [children]
              (->> children
                   (map (fn [child]
                          (assoc child
                                 :available-width java.lang.Integer/MAX_VALUE
                                 :available-height java.lang.Integer/MAX_VALUE)))))))

(defn- grid-make-layout [node]
  (assoc node :children
         (let [column-widths (->> (:children node)
                                  (group-by ::column)
                                  (medley/map-vals (fn [column]
                                                     (apply max (map :width column)))))
               rows (partition-by ::row (:children node))]
           (loop [layouted-nodes []
                  x 0
                  y 0
                  cells (first rows)
                  rows (rest rows)
                  max-height 0]
             (if-let [cell (first cells)]
               (recur (conj layouted-nodes
                            (assoc cell
                                   :x x
                                   :y y))
                      (+ x (get column-widths (::column cell)))
                      y
                      (rest cells)
                      rows
                      (max max-height (:height cell)))
               (if (not (empty? rows))
                 (recur layouted-nodes
                        0
                        (+ y max-height)
                        (first rows)
                        (rest rows)
                        0)
                 layouted-nodes))))))

(defn grid [rows]
  {:type ::grid
   :children (apply concat (map-indexed (fn [row-number row]
                                          (map-indexed (fn [column-number cell]
                                                         (assoc cell
                                                                ::column column-number
                                                                ::row row-number))
                                                       row))
                                        rows))
   :get-size grid-get-size
   :give-space grid-give-space
   :make-layout grid-make-layout})

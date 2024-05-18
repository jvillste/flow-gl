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

(defn get-first-child-size [node _available-width _available-height]
  (measuring/size (first (:children node))))

;; vertically

(defn- vertical-stack-get-size [node _available-width _available-height]
  {:width (apply max
                 (conj (remove nil? (map :width (:children node)))
                       0))
   :height (if (empty? (:children node))
             0
             (+ (* (dec (count (:children node)))
                   (:margin node))
                (reduce + (map :height (:children node)))))})

(defn- vertical-stack-make-layout [node]
  (let [maximum-given-width (when (not (:fill-width? node))
                              (min (:width node)
                                   (apply max (conj (remove nil? (map :given-width (:children node)))
                                                    0))))]
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
                                   :y y
                                   :width (if (not (:fill-width? node))
                                            maximum-given-width
                                            (:width child))))
                      (+ y (:height child)
                         (:margin node))
                      (rest children))
               layouted-nodes)))))

(defn vertical-stack-available-area-for-children [vertical-stack available-width _available-height]
  (repeat (count (:children vertical-stack))
          {:available-width available-width
           :available-height java.lang.Integer/MAX_VALUE}))

(def vertical-stack
  {:type ::vertical-stack
   :get-size vertical-stack-get-size
   :available-area-for-children vertical-stack-available-area-for-children
   :make-layout vertical-stack-make-layout
   :fill-width? true})

(defn vertically [& children]
  (assoc vertical-stack
         :margin 0
         :children (flatten-contents children)))

(defn vertically-2 [{:keys [centered?
                            fill-width?
                            margin]
                     :or {centered? false
                          fill-width? true
                          margin 0}}
                    & children]
  (assoc vertical-stack
         :margin margin
         ::centered centered?
         :fill-width? fill-width?
         :children (flatten-contents children)))

(defn vertically-with-margin [margin & children]
  (assoc vertical-stack
         :margin margin
         :children (flatten-contents children)))

(defn wrap
  "adds a node to the scene graph without affecting the layout"
  [child]
  {:children [child]
   :get-size (fn [node _available-width _available-height]
               {:width (or (:width (first (:children node)))
                           0)
                :height (or (:height (first (:children node)))
                            0)})})

(defn- horizontal-stack-get-size
  [node _available-width _available-height]
  {:width (if (empty? (:children node))
            0
            (+ (* (dec (count (:children node)))
                  (:margin node))
               (reduce + (map :width (:children node)))))
   :height (apply max
                  (conj (map :height (:children node))
                        0))})

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

(defn horizontal-stack-available-area-for-children [node _available-width available-height]
  (repeat (count (:children node))
          {:available-width java.lang.Integer/MAX_VALUE
           :available-height available-height}))

(def horizontal-stack
  {:type ::horizontal-stack
   :get-size horizontal-stack-get-size
   :available-area-for-children horizontal-stack-available-area-for-children
   ;; TODO: how to give each child only remaining space? is their size known here?
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
  [node available-width _available-height]
  {:width available-width
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
  [node _available-width available-height]
  {:width (:width (first (:children node)))
   :height available-height})

(defn center-vertically [child]
  {:make-layout center-vertically-make-layout
   :children [child]
   :get-size center-vertically-get-size})

;; box

(spec/def ::margin int?)

(defn box-get-size [{:keys [margin children]} _available-width _available-height]
  (let [[outer inner] children]
    {:width (+ (* 2 margin)
               (:width inner))
     :height (+ (* 2 margin)
                (:height inner))}))


(defn box-available-area-for-children [node available-width available-height]
  [{:available-width available-width
    :available-height available-height}
   {:available-width (- available-width
                        (* 2 (:margin node)))
    :available-height (- available-height
                         (* 2 (:margin node)))}])

(defn box-make-layout [{:keys [width height margin fill-width?] :as node}]
  (update-in node
             [:children]
             (fn [[outer inner]]
               [(assoc outer
                       :x 0
                       :y 0
                       :width (if fill-width?
                                width
                                (max width
                                     (+ (:width inner)
                                        (* 2 margin))))
                       :height (max height
                                    (+ (:height inner)
                                       (* 2 margin))))
                (assoc inner
                       :x margin
                       :y margin)])))


(defn box [margin outer inner & [{:keys [fill-width?] :or {fill-width? false}}]]
  (when (and outer inner)
    {:type ::box
     :fill-width? fill-width?
     :get-size box-get-size
     :available-area-for-children box-available-area-for-children
     :make-layout box-make-layout
     :margin margin
     :children [outer inner]}))


;; hover

(defn hover-available-area-for-children [node _available-width _available-height]
  (repeat (count (:children node))
          {:available-width java.lang.Integer/MAX_VALUE
           :available-height java.lang.Integer/MAX_VALUE}))

(defn- hover-get-size
  [_node _available-width _available-height]
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
    :available-area-for-children hover-available-area-for-children
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

(defn scale-get-size [node _available-width _available-height]
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

(defn get-limited-size [{:keys [width-limit height-limit compare-function children]} _available-width _available-height]
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

(defn give-limited-available-area-for-children [{:keys [width-limit height-limit compare-function]} available-width available-height]
  [{:available-width (if width-limit
                       (compare-function width-limit
                                         available-width)
                       available-width)
    :available-height (if height-limit
                        (compare-function height-limit
                                          available-height)
                        available-height)}])

(defn with-minimum-size [minimum-width minimum-height child]
  (when child
    {:type ::with-minimum-size
     :get-size get-limited-size
     :available-area-for-children give-limited-available-area-for-children
     :make-layout make-limited-layout
     :compare-function max

     :width-limit minimum-width
     :height-limit minimum-height

     :children [child]}))


;; with-maximum-size

(defn with-maximum-size [maximum-width maximum-height child]
  (when child
    {:get-size get-limited-size
     :make-layout make-limited-layout
     :available-area-for-children give-limited-available-area-for-children
     :width-limit maximum-width
     :height-limit maximum-height
     :compare-function min

     :children [child]}))


;; with-margins

(defn with-margins-get-size [{:keys [left-margin right-margin top-margin bottom-margin children]} _available-width _available-height]
  (let [child (first children)]
    {:width (+ left-margin
               right-margin
               (:width child))
     :height (+ top-margin
                bottom-margin
                (:height child))}))

(defn with-margins-available-area-for-children [{:keys [left-margin right-margin top-margin bottom-margin]} available-width available-height]
  [{:available-width (- available-width
                        left-margin
                        right-margin)
    :available-height (- available-height
                         top-margin
                         bottom-margin)}])

(defn with-margins-make-layout [{:keys [left-margin top-margin] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(assoc child
                       :x (+ (or (:given-x child)
                                 0)
                             left-margin)
                       :y (+ (or (:given-x child)
                                 0)
                             top-margin))])))

(defn with-margins [top-margin right-margin bottom-margin left-margin child]
  (when child
    {:type ::with-margins
     :get-size with-margins-get-size
     :make-layout with-margins-make-layout
     :available-area-for-children with-margins-available-area-for-children
     :top-margin top-margin
     :right-margin right-margin
     :bottom-margin bottom-margin
     :left-margin left-margin

     :children [child]}))

(defn with-margin [margin child]
  (when child
    {:type ::with-margins
     :get-size with-margins-get-size
     :available-area-for-children with-margins-available-area-for-children
     :make-layout with-margins-make-layout

     :top-margin margin
     :right-margin margin
     :bottom-margin margin
     :left-margin margin

     :children [child]}))


;; superimpose

(defn superimpose-get-size [node _available-width _available-height]
  (let [child-sizes (map (fn [child]
                           (merge (select-keys child [:x :y :width :height])
                                  #_(measuring/size child)))
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
      (let [node-size (measuring/size node
                                      maximum-width
                                      java.lang.Integer/MAX_VALUE)]
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

(defn make-flow-row-layout [nodes y width height]
  (loop [x 0
         nodes nodes
         layouted-nodes []]
    (if-let [node (first nodes)]
      (let [preferred-size (measuring/size node width height)]
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
             (let [row (flow-row children (:width node))]
               (recur (concat layouted-children (make-flow-row-layout (:row-nodes row)
                                                                      y
                                                                      (:width node)
                                                                      (:height row)))
                      (+ y
                         (:height row))
                      (:unused-nodes row)))
             layouted-children))))

(defn get-flow-size [node available-width _available-height]
  {:width available-width
   :height (loop [y 0
                  children (:children node)]
             (if (seq children)
               (let [row (flow-row children available-width)]
                 (recur (+ y
                           (:height row))
                        (:unused-nodes row)))
               y))})


(defn flow [& children]
  {:get-size get-flow-size
   :make-layout make-flow-layout
   :children (flatten-contents children)})


;; vertical-split

(defn- vertical-split-get-size [_node available-width available-height]
  {:width available-width
   :height available-height})

(defn- vertical-split-available-area-for-children [node available-width available-height]
  (let [row-height (/ available-height
                      (count (:children node)))]
    (repeat (count (:children node))
            {:available-width available-width
             :available-height row-height})))

(defn- vertical-split-make-layout [node]
  (let [row-height (/ (:height node)
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
   :available-area-for-children vertical-split-available-area-for-children
   :make-layout vertical-split-make-layout})


;; grid

(defn- grid-get-size [node _available-width _available-height]
  {:width (->> (group-by ::column (:children node))
               (vals)
               (map (fn [column-nodes]
                      (apply max (map :width column-nodes))))
               (reduce +))
   :height (->> (group-by ::row (:children node))
                (vals)
                (map (fn [row-nodes]
                       (apply max (map :height row-nodes))))
                (reduce +))})

(defn- grid-available-area-for-children [node _available-width _available-height]
  (repeat (count (:children node))
          {:available-width java.lang.Integer/MAX_VALUE
           :available-height java.lang.Integer/MAX_VALUE}))

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
                                                         {::column column-number
                                                          ::row row-number
                                                          :node cell})
                                                       row))
                                        rows))
   :get-size grid-get-size
   :available-area-for-children grid-available-area-for-children
   :make-layout grid-make-layout})

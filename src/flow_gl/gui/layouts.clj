(ns flow-gl.gui.layouts
  (:require  [clojure.spec.alpha :as spec]
             (flow-gl.gui [layout :as layout]
                          [layoutable :as layoutable]
                          [drawable :as drawable]
                          [cache :as cache]))
  (:use clojure.test))


(layout/deflayout FixedSize [width height children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children 0]
                     #(layout/set-dimensions-and-layout % 0 0 width height)))

  (preferred-size [this available-width available-height]
                  {:width width :height height}))

(layout/deflayout MinimumSize [width height children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children 0]
                     #(let [preferred-size (layoutable/preferred-size % requested-width requested-height)]
                        (layout/set-dimensions-and-layout % 0 0
                                                          (max width (:width preferred-size))
                                                          (max height (:height preferred-size))))))

  (preferred-size [this available-width available-height]
                  (let [preferred-size (layoutable/preferred-size (first children) available-width available-height)]
                    {:width (max width (:width preferred-size))
                     :height (max height (:height preferred-size))})))

(layout/deflayout FloatRight [children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children]
                     (fn [[left right]]
                       (let [right-width (:width (layoutable/preferred-size right java.lang.Integer/MAX_VALUE requested-height))
                             left-width (- requested-width right-width)]
                         [(layout/set-dimensions-and-layout left 0 0 left-width requested-height)
                          (layout/set-dimensions-and-layout right left-width 0 right-width requested-height)]))))

  (preferred-size [this available-width available-height]
                  (let [[left right] children
                        right-size (layoutable/preferred-size right available-width available-height)
                        left-size (layoutable/preferred-size left (- available-width (:width right-size)) available-height)]
                    {:width (+ (:width left-size)
                               (:width right-size))

                     :height (max (:height left-size)
                                  (:height right-size))})))

(layout/deflayout FloatLeft [children]
  (layout [this requested-width requested-height]
          (update-in this [:children]
                     (fn [[left right]]
                       (let [left-width (:width (layoutable/preferred-size left java.lang.Integer/MAX_VALUE requested-height))
                             right-width (- requested-width left-width)]
                         [(layout/set-dimensions-and-layout left 0 0 left-width requested-height)
                          (layout/set-dimensions-and-layout right left-width 0 right-width requested-height)]))))

  (preferred-size [this available-width available-height]
                  (let [[left right] children
                        left-size (layoutable/preferred-size left available-width available-height)
                        right-size (layoutable/preferred-size right (- available-width (:width left-size)) available-height)]
                    {:width (+ (:width left-size)
                               (:width right-size))

                     :height (max (:height left-size)
                                  (:height right-size))})))

(layout/deflayout FloatTop [children]
  (layout [this requested-width requested-height]
          (update-in this [:children]
                     (fn [[top bottom]]
                       (let [top-height (:height (layoutable/preferred-size top requested-width java.lang.Integer/MAX_VALUE))
                             bottom-height (- requested-height top-height)]
                         [(layout/set-dimensions-and-layout top 0 0 requested-width top-height)
                          (layout/set-dimensions-and-layout bottom 0 top-height requested-width bottom-height)]))))

  (preferred-size [this available-width available-height]
                  (let [[top bottom] children
                        top-size (layoutable/preferred-size top available-width available-height)
                        bottom-size (layoutable/preferred-size bottom available-width (- available-height (:height top-size)))]
                    {:width (max (:width top-size)
                                 (:width bottom-size))
                     :height (+ (:height top-size)
                                (:height bottom-size))})))

(layout/deflayout Box [margin children]
  (layout [box requested-width requested-height]
          (-> box
              (update-in [:children]
                         (fn [[outer inner]]
                           (let [inner-size (layoutable/preferred-size inner (- requested-width (* 2 margin)) (- requested-height (* 2 margin)))]
                             [(assoc (layout/set-dimensions-and-layout outer 0 0 requested-width requested-height)
                                     :z 0)
                              (assoc (layout/set-dimensions-and-layout inner margin margin (- requested-width (* 2 margin)) (- requested-height (* 2 margin)) #_(:width inner-size) #_(:height inner-size))
                                     :z 1)])))))

  (preferred-size [this available-width available-height]
                  (let [child-size (layoutable/preferred-size (second children) (- available-width (* 2 margin)) (- available-height (* 2 margin)))]
                    {:width (+ (* 2 margin)
                               (:width child-size))
                     :height (+ (* 2 margin)
                                (:height child-size))})))

(layout/deflayout Preferred [children]
  (layout [this requested-width requested-height]
          (assoc this :children (let [child (first children)
                                      preferred-size (layoutable/preferred-size child requested-width requested-height)]
                                  [(layout/set-dimensions-and-layout child
                                                                     0
                                                                     0
                                                                     (min requested-width
                                                                          (:width preferred-size))
                                                                     (min requested-height
                                                                          (:height preferred-size)))])))

  (preferred-size [this available-width available-height]
                  (layoutable/preferred-size (first children) available-width available-height)))


(layout/deflayout Margin [margin-top margin-right margin-bottom margin-left children]
  (layout [this requested-width requested-height]
          (update-in this [:children] (fn [[layoutable]]
                                        [(layout/set-dimensions-and-layout layoutable
                                                                           margin-left
                                                                           margin-top
                                                                           (- requested-width margin-left margin-right)
                                                                           (- requested-height margin-top margin-bottom))])))

  (preferred-size [this available-width available-height]
                  (let [child-size (layoutable/preferred-size (first children)
                                                              (- available-width
                                                                 margin-left
                                                                 margin-right)
                                                              (- available-height
                                                                 margin-top
                                                                 margin-bottom))]
                    {:width (+ margin-left
                               margin-right
                               (:width child-size))
                     :height (+ margin-top
                                margin-bottom
                                (:height child-size))})))

(layout/deflayout Absolute [children]
  (layout [absolute requested-width requested-height]
          (assoc absolute :children
                 (vec (map (fn [child]
                             (let [child-size (layoutable/preferred-size child java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE)]
                               (layout/set-dimensions-and-layout child (or (:x child) 0) (or (:y child) 0) (:width child-size) (:height child-size))))
                           children))))

  (preferred-size [this available-width available-height]
                  (let [child-sizes (map (fn [child]
                                           (-> (layoutable/preferred-size child java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE)
                                               (assoc :x (:x child))
                                               (assoc :y (:y child))))
                                         children)]
                    {:width (apply max (map (fn [child-size]
                                              (+ (:x child-size)
                                                 (:width child-size)))
                                            child-sizes))
                     :height (apply max (map (fn [child-size]
                                               (+ (:y child-size)
                                                  (:height child-size)))
                                             child-sizes))})))

(layout/deflayout VerticalStack [children]
  (layout [vertical-stack requested-width requested-height]
          (assoc vertical-stack :children
                 (loop [layouted-layoutables []
                        y 0
                        children children]
                   (if (seq children)
                     (let [height (:height (layoutable/preferred-size (first children)
                                                                      requested-width
                                                                      java.lang.Integer/MAX_VALUE))]
                       (recur (conj layouted-layoutables
                                    (layout/set-dimensions-and-layout (first children)
                                                                      0
                                                                      y
                                                                      requested-width
                                                                      (min height
                                                                           (max 0
                                                                                (- requested-height
                                                                                   y)))))
                              (+ y height)
                              (rest children)))
                     layouted-layoutables))))

  (preferred-size [vertical-stack available-width available-height]
                  (let [child-sizes (map (fn [child]
                                           (layoutable/preferred-size child
                                                                      available-width
                                                                      java.lang.Integer/MAX_VALUE))
                                         children)]
                    {:width (apply max
                                   (conj (map :width child-sizes)
                                         0))
                     :height (reduce + (map :height child-sizes))})))


(layout/deflayout HorizontalStack [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (loop [layouted-layoutables []
                        x 0
                        children children]
                   (if (seq children)
                     (let [width (:width (layoutable/preferred-size (first children) java.lang.Integer/MAX_VALUE requested-height))]
                       (recur (conj layouted-layoutables (layout/set-dimensions-and-layout (first children)
                                                                                           x
                                                                                           0
                                                                                           width
                                                                                           requested-height))
                              (+ x width)
                              (rest children)))
                     layouted-layoutables))))

  (preferred-size [this available-width available-height]
                  (let [child-sizes (map (fn [child]
                                           (layoutable/preferred-size child java.lang.Integer/MAX_VALUE available-height))
                                         children)]
                    {:height (apply max (conj (map :height child-sizes)
                                              0))
                     :width (reduce + (map :width child-sizes))})))

(defn table-sizes [rows]
  (loop [column-widths (vec (repeat (count (first rows)) 0))
         row-heights []
         rows rows]
    (if-let [row (first rows)]
      (let [[column-widths row-height]
            (loop [column-number 0
                   cells row
                   column-widths column-widths
                   row-height 0]
              (if-let [cell (first cells)]
                (let [preferred-size (layoutable/preferred-size cell
                                                                java.lang.Integer/MAX_VALUE
                                                                java.lang.Integer/MAX_VALUE)]
                  
                  (recur (inc column-number)
                         (rest cells)
                         (update-in column-widths [column-number] #(max % (:width preferred-size)))
                         (max row-height (:height preferred-size))))
                [column-widths
                 row-height]))]
        (recur column-widths
               (conj row-heights row-height)
               (rest rows)))
      [column-widths
       row-heights])))


#_(table-sizes [[(flow-gl.gui.controls/text "foo") (flow-gl.gui.controls/text "foo")]
                [(flow-gl.gui.controls/text "foo bar") (flow-gl.gui.controls/text "foo")]])

(defn layout-row [layoutables y height widths]
  (let [default-width (last widths)]
    (loop [layouted-layoutables []
           x 0
           layoutables layoutables
           widths widths]
      (if-let [layoutable (first layoutables)]
        (recur (conj layouted-layoutables (layout/set-dimensions-and-layout layoutable
                                                                            x
                                                                            y
                                                                            (or (first widths)
                                                                                default-width)
                                                                            height))
               (+ x (first widths))
               (rest layoutables)
               (rest widths))
        layouted-layoutables))))

#_(layout-row [(flow-gl.gui.controls/text "foo") (flow-gl.gui.controls/text "foo")]
              0
              10
              [20 30])

(layout/deflayout Table [children column-count]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (let [rows (partition column-count
                                       children)
                       [column-widths row-heights] (table-sizes rows)]
                   (loop [rows rows
                          row-heights row-heights
                          y 0
                          layouted-children []]
                     (if-let [row (first rows)]
                       (recur (rest rows)
                              (rest row-heights)
                              (+ y (first row-heights))
                              (concat layouted-children
                                      (layout-row row
                                                  y
                                                  (first row-heights)
                                                  column-widths)))
                       layouted-children)))))

  (preferred-size [this available-width available-height]
                  (let [rows (partition column-count
                                        children)
                        [column-widths row-heights] (table-sizes rows)]
                    {:height (reduce + row-heights)
                     :width (reduce + column-widths)})))

(defn static-table-layout [this requested-width requested-height
                           rows column-widths row-height]
  (assoc this :children
         (loop [rows rows
                y 0
                layouted-children []]
           (if-let [row (first rows)]
             (recur (rest rows)
                    (+ y row-height)
                    (concat layouted-children
                            (layout-row row
                                        y
                                        row-height
                                        column-widths)))
             layouted-children))))

(defn static-table-preferred-size [this available-width available-height
                                   rows column-widths row-height]
  {:height (* (count rows)
              row-height)
   ;;(reduce + row-heights)
   :width (reduce + column-widths)})

(defn static-table-children-in-coordinates [rows row-height column-widths x y]
  (let [row (int (/ y row-height))
        column (loop [column-x 0
                      column 0]
                 (let [column-width (nth column-widths
                                         column)]
                   (if (< x
                          (+ column-x
                             column-width))
                     column
                     (recur (+ column-x
                               column-width)
                            (inc column)))))]
    
    [(+ (* row (count (first rows)))
        column)]))

(deftest static-table-children-in-coordinates-test
  (is (= [0]
         (static-table-children-in-coordinates [[1 2 3]] 10 [10 10 10] 5 5)))

  (is (= [1]
         (static-table-children-in-coordinates [[1 2 3]] 10 [10 10 10] 15 5)))

  (is (= [4]
         (static-table-children-in-coordinates [[1 2 3]] 10 [10 10 10] 15 15))))

(run-tests)

(defrecord StaticTable [rows column-widths row-height]
  layout/Layout
  (layout [layoutable state requested-width requested-height]
    (binding [layout/current-state-atom (atom state)]
      (let [layout (cache/call-with-cache-atom layout/cache static-table-layout
                                               layoutable requested-width requested-height
                                               rows column-widths row-height)]
        [@layout/current-state-atom
         layout])))

  (children [this]
    (apply concat rows))

  layoutable/Layoutable
  (layoutable/preferred-size [this available-width available-height]
    (cache/call-with-cache-atom layout/cache static-table-preferred-size this available-width available-height
                                rows column-widths row-height))

  layout/SpatialIndex
  (children-in-coordinates [this x y]
    (static-table-children-in-coordinates rows row-height column-widths x y)))

#_(layout/deflayout StaticTable [children column-count column-widths row-heights]
    (layout [this requested-width requested-height]
            (assoc this :children
                   (let [rows (partition column-count
                                         children)
                         default-height (last row-heights)]
                     (loop [rows rows
                            row-heights row-heights
                            y 0
                            layouted-children []]
                       (if-let [row (first rows)]
                         (recur (rest rows)
                                (rest row-heights)
                                (+ y (first row-heights))
                                (concat layouted-children
                                        (layout-row row
                                                    y
                                                    (or (first row-heights)
                                                        default-height)
                                                    column-widths)))
                         layouted-children)))))

    ;; TODO: handle the case when there is more rows or columns than row heights and column widths
    (preferred-size [this available-width available-height]
                    {:height (* (mod (count children)
                                     column-count)
                                (first row-heights))
                     ;;(reduce + row-heights)
                     :width (reduce + column-widths)}))



(defn flow-row [layoutables maximum-width]
  (loop [height 0
         row-width 0
         row-layoutables []
         remaining-layoutables layoutables]
    (if-let [layoutable (first remaining-layoutables)]
      (let [layoutable-size (layoutable/preferred-size layoutable java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE)]
        (if (and (> (+ row-width
                       (:width layoutable-size))
                    maximum-width)
                 (not (empty? row-layoutables)))
          {:row-layoutables row-layoutables
           :height height
           :unused-layoutables remaining-layoutables}
          (recur (max height (:height layoutable-size))
                 (+ row-width
                    (:width layoutable-size))
                 (conj row-layoutables layoutable)
                 (rest remaining-layoutables))))
      {:row-layoutables row-layoutables
       :height height
       :unused-layoutables remaining-layoutables})))

(defn layout-flow-row [layoutables y height]
  (loop [x 0
         layoutables layoutables
         layouted-layoutables []]
    (if-let [layoutable (first layoutables)]
      (let [preferred-size (layoutable/preferred-size layoutable
                                                      java.lang.Integer/MAX_VALUE
                                                      java.lang.Integer/MAX_VALUE)]
        (recur (+ x
                  (:width preferred-size))
               (rest layoutables)
               (conj layouted-layoutables
                     (layout/set-dimensions-and-layout layoutable
                                                       x
                                                       y
                                                       (:width preferred-size)
                                                       height))))
      layouted-layoutables)))

(layout/deflayout Flow [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (loop [layouted-layoutables []
                        y 0
                        children children]
                   (if (seq children)
                     (let [row (flow-row children requested-width)]
                       (recur (concat layouted-layoutables (layout-flow-row (:row-layoutables row)
                                                                            y
                                                                            (:height row)))
                              (+ y
                                 (:height row))
                              (:unused-layoutables row)))
                     layouted-layoutables))))

  (preferred-size [this available-width available-height]
                  {:width available-width
                   :height (loop [y 0
                                  children children]
                             (if (seq children)
                               (let [row (flow-row children available-width)]
                                 (recur (+ y
                                           (:height row))
                                        (:unused-layoutables row)))
                               y))}))

(layout/deflayout BottomOutOfLayout [children]
  (layout [this requested-width requested-height]
          (let [childs-preferred-size (layoutable/preferred-size (first children)
                                                                 requested-width requested-height)]
            (assoc this :children [(-> (layout/set-dimensions-and-layout (first children)
                                                                         0
                                                                         0
                                                                         (:width childs-preferred-size)
                                                                         (:height childs-preferred-size))
                                       (assoc :z 1
                                              :out-of-layout true))])))
  
  (preferred-size [this available-width available-height]
                  {:width (:width (layoutable/preferred-size (first children)
                                                             available-width available-height))
                   :height 0}))



(defn size-group-width [size-group]
  (apply max (conj (map (fn [element]
                          (:width (layoutable/preferred-size element
                                                             java.lang.Integer/MAX_VALUE
                                                             java.lang.Integer/MAX_VALUE)))
                        (:members size-group) )
                   0)))

(defn size-group-height [size-group]
  (apply max (conj (map (fn [element]
                          (:height (layoutable/preferred-size element
                                                              java.lang.Integer/MAX_VALUE
                                                              java.lang.Integer/MAX_VALUE)))
                        (:members size-group))
                   0)))

(layout/deflayout SizeGroupMember [size-group mode children]
  (layout [this requested-width requested-height]
          (let [size (layoutable/preferred-size (first children) requested-width requested-height)]
            (assoc this :children
                   [(layout/set-dimensions-and-layout (first children)
                                                      0
                                                      0
                                                      (:width size)
                                                      (:height size))])))

  (preferred-size [this available-width available-height]
                  (let [child-size (layoutable/preferred-size (first children) available-width available-width)]
                    {:width (if (#{:width :both} mode)
                              (cache/call-with-cache-atom layout/cache size-group-width @size-group)
                              (:width child-size))
                     :height (if (#{:height :both} mode)
                               (cache/call-with-cache-atom layout/cache size-group-height @size-group)
                               (:height child-size))})))

(defn create-size-group []
  (atom {:members #{}}))

(defn size-group-member [size-group mode layoutable]
  (swap! size-group #(update-in % [:members] conj layoutable))
  (->SizeGroupMember size-group mode [layoutable]))

(defn grid [rows]
  (let [size-groups (repeatedly create-size-group)]
    (->VerticalStack
     (for [row rows]
       (->HorizontalStack (for [[cell size-group] (partition 2
                                                             (interleave row
                                                                         size-groups))]
                            (size-group-member size-group :width cell)))))))


#_(deflayout Stack [children]
    (layout [this requested-width requested-height]
            (assoc this :children
                   (vec (map #(layout/set-dimensions-and-layout % 0 0 requested-width requested-height)
                             children))))

    (preferred-width [this]
                     (apply max (conj (map layoutable/preferred-width children)
                                      0)))

    (preferred-height [this]
                      (apply max (conj (map layoutable/preferred-height children)
                                       0))))

(layout/deflayout Superimpose [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (vec (map-indexed (fn [index child]
                                     (let [preferred-child-size (layoutable/preferred-size child requested-width requested-height)]
                                       (assoc (layout/set-dimensions-and-layout child
                                                                                0
                                                                                0
                                                                                (:width preferred-child-size)
                                                                                (:height preferred-child-size))
                                              :z index)))
                                   children))))

  (preferred-size [this available-width available-height]
                  (let [child-sizes (map (fn [child]
                                           (layoutable/preferred-size child java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE))
                                         children)]
                    {:width (apply max (map :width
                                            child-sizes))
                     :height (apply max (map :height
                                             child-sizes))})))

(layout/deflayout Center [children]
  (layout [this requested-width requested-height]
          (let [child (first children)
                preferred-child-size (layoutable/preferred-size child requested-width requested-height)]
            (assoc this :children
                   [(layout/set-dimensions-and-layout child
                                                      (- (/ requested-width 2)
                                                         (/ (:width preferred-child-size) 2))
                                                      (- (/ requested-height 2)
                                                         (/ (:height preferred-child-size) 2))
                                                      (:width preferred-child-size)
                                                      (:height preferred-child-size))])))

  (preferred-size [this available-width available-height]
                  (layoutable/preferred-size (first children) available-width available-height)))




(layout/deflayout Translate [translate-x translate-y child]

  (layout [this requested-width requested-height]
          (let [preferred-child-size (layoutable/preferred-size child requested-width requested-height)]
            (assoc this :children
                   [(layout/set-dimensions-and-layout child
                                                      translate-x
                                                      translate-y
                                                      (:width preferred-child-size)
                                                      (:height preferred-child-size))])))

  (preferred-size [this available-width available-height]
                  (let [preferred-child-size (layoutable/preferred-size child available-width available-height)]
                    {:width (+ translate-x (:width preferred-child-size))
                     :height (+ translate-y (:height preferred-child-size))})))


#_(deflayout DockBottom [layoutable]

    (layout [dock-bottom requested-width requested-height]
            (let [height (layoutable/preferred-height layoutable )]
              (assoc dock-bottom :layoutable
                     (layout/set-dimensions-and-layout layoutable
                                                       0
                                                       (- requested-height
                                                          height)
                                                       (layoutable/preferred-width layoutable)
                                                       height))))

    (preferred-width [dock-bottom] (layoutable/preferred-width layoutable))

    (preferred-height [dock-bottom] (layoutable/preferred-height layoutable)))


#_(run-tests)

(defn flatten-contents [values]
  (->> values
       (filter (complement nil?))
       (flatten)
       (filter (complement nil?))))

(def vertical-stack
  {:get-size (fn [node]
               {:width (apply max
                              (conj (map :width (:children node))
                                    0))
                :height (reduce + (map :height (:children node)))})
   
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
                                    (+ y (:height child))
                                    (rest children))
                             layouted-nodes))))})

(defn vertically [& children]
  (assoc vertical-stack
         :children (flatten-contents children)))


(def horizontal-stack
  {:get-size (fn [node]
               {:width (reduce + (map :width (:children node)))
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
                                    (+ x (:width child))
                                    (rest children))
                             layouted-nodes))))})

(defn horizontally [& children]
  (assoc horizontal-stack
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


;; with-minimum-size

(spec/def ::minimum-width int?)
(spec/def ::minimum-height int?)

(defn minimum-size-get-size [{:keys [minimum-width minimum-height children]}]
  (spec/assert ::minimum-width minimum-width)
  (spec/assert ::minimum-height minimum-height)
  
  {:width (max minimum-width
               (:width (first children)))
   :height (max minimum-height
                (:height (first children)))} )

(defn minimum-size-make-layout [{:keys [width height] :as node}]
  (update-in node
             [:children]
             (fn [[child]]
               [(layout/add-layout (assoc child
                                          :x 0
                                          :y 0
                                          :z 0
                                          :width width
                                          :height height))])))

(defn with-minimum-size [minimum-width minimum-height child]
  (when child
    {:get-size minimum-size-get-size
     :make-layout minimum-size-make-layout

     :minimum-width minimum-width
     :minimum-height minimum-height
     
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

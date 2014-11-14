(ns flow-gl.gui.layouts
  (:require  (flow-gl.gui [layout :as layout]
                          [gui :as gui]
                          [layoutable :as layoutable]
                          [drawable :as drawable]))
  (:use clojure.test))


(layout/deflayout-not-memoized FixedSize [width height children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children 0]
                     #(layout/set-dimensions-and-layout % 0 0 width height)))

  (preferred-size [this available-width available-height]
                  {:width width :height height}))

(layout/deflayout-not-memoized FloatRight [children]
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

(layout/deflayout-not-memoized FloatLeft [left right]

  (layout [this requested-width requested-height]
          (let [left-width (:width (layoutable/preferred-size left java.lang.Integer/MAX_VALUE requested-height))
                right-width (- requested-width left-width)]
            (assoc this :children
                   [(layout/set-dimensions-and-layout left 0 0 left-width requested-height)
                    (layout/set-dimensions-and-layout right left-width 0 right-width requested-height)])))

  (preferred-size [this available-width available-height]
                  (let [left-size (layoutable/preferred-size left available-width available-height)
                        right-size (layoutable/preferred-size right (- available-width (:width left-size)) available-height)]
                    {:width (+ (:width left-size)
                               (:width right-size))

                     :height (max (:height left-size)
                                  (:height right-size))})))

(layout/deflayout-not-memoized FloatTop [top bottom]
  (layout [this requested-width requested-height]
          (let [top-height (:height (layoutable/preferred-size top requested-width java.lang.Integer/MAX_VALUE))
                bottom-height (- requested-height top-height)]
            (assoc this :children
                   [(layout/set-dimensions-and-layout top 0 0 requested-width top-height)
                    (layout/set-dimensions-and-layout bottom 0 top-height requested-width bottom-height)])))

  (preferred-size [this available-width available-height]
                  (let [top-size (layoutable/preferred-size top available-width available-height)
                        bottom-size (layoutable/preferred-size bottom available-width (- available-height (:height top-size)))]
                    {:width (max (:width top-size)
                                 (:width bottom-size))
                     :height (+ (:height top-size)
                                (:height bottom-size))})))

(layout/deflayout-not-memoized Box [margin children]
  (layout [box requested-width requested-height]
          (-> box
              (update-in [:children]
                         (fn [[outer inner]]
                           (let [inner-size (layoutable/preferred-size inner (- requested-width (* 2 margin)) (- requested-height (* 2 margin)))]
                             [(layout/set-dimensions-and-layout outer 0 0 requested-width requested-height)
                              (assoc (layout/set-dimensions-and-layout inner margin margin (:width inner-size) (:height inner-size))
                                :z 1)])))))

  (preferred-size [this available-width available-height]
                  (let [child-size (layoutable/preferred-size (second children) (- available-width (* 2 margin)) (- available-height (* 2 margin)))]
                    {:width (+ (* 2 margin)
                               (:width child-size))
                     :height (+ (* 2 margin)
                                (:height child-size))})))

(layout/deflayout-not-memoized Preferred [child]
  (layout [this requested-width requested-height]
          (assoc this :children (let [preferred-size (layoutable/preferred-size child requested-width requested-height)]
                                  [(layout/set-dimensions-and-layout child
                                                                     0
                                                                     0
                                                                     (:width preferred-size)
                                                                     (:height preferred-size))])))

  (preferred-size [this available-width available-height]
                  (layoutable/preferred-size child available-width available-height)))


(layout/deflayout-not-memoized Margin [margin-top margin-right margin-bottom margin-left children]
  (layout [this requested-width requested-height]
          (update-in this [:children] (fn [[layoutable]]
                                        [(layout/set-dimensions-and-layout layoutable
                                                                           margin-left
                                                                           margin-top
                                                                           (- requested-width margin-left margin-right)
                                                                           (- requested-height margin-top margin-bottom))])))

  (preferred-size [this available-width available-height]
                  (let [child-size (layoutable/preferred-size (first children) (- available-width margin-left margin-right) (- available-height margin-top margin-bottom))]
                    {:width (+ margin-left margin-right
                               (:width child-size))
                     :height (+ margin-top margin-bottom
                                (:height child-size))})))

(layout/deflayout-not-memoized Absolute [children]
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

(layout/deflayout-not-memoized VerticalStack [children]
  (layout [vertical-stack requested-width requested-height]
          (assoc vertical-stack :children
                 (loop [layouted-layoutables []
                        y 0
                        children children]
                   (if (seq children)
                     (let [height (:height (layoutable/preferred-size (first children) requested-width java.lang.Integer/MAX_VALUE))]
                       (recur (conj layouted-layoutables (layout/set-dimensions-and-layout (first children)
                                                                                           0
                                                                                           y
                                                                                           requested-width
                                                                                           height))
                              (+ y height)
                              (rest children)))
                     layouted-layoutables))))

  (preferred-size [vertical-stack available-width available-height]
                  (let [child-sizes (map (fn [child]
                                           (layoutable/preferred-size child available-width java.lang.Integer/MAX_VALUE))
                                         children)]
                    {:width (apply max (conj (map :width child-sizes)
                                             0))
                     :height (reduce + (map :height child-sizes))})))


(layout/deflayout-not-memoized HorizontalStack [children]
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

(defn layout-row [layoutables y height]
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

(layout/deflayout-not-memoized Flow [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (loop [layouted-layoutables []
                        y 0
                        children children]
                   (if (seq children)
                     (let [row (flow-row children requested-width)]
                       (recur (concat layouted-layoutables (layout-row (:row-layoutables row)
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

#_(deflayout BottomOutOfLayout [children]
    (layout [this requested-width requested-height]
            (assoc this :children [(-> (layout/set-dimensions-and-layout (first children)
                                                                         0
                                                                         0
                                                                         (layoutable/preferred-width (first children))
                                                                         (layoutable/preferred-height (first children)))
                                       (assoc :z 1
                                              :out-of-layout true))]))

    (preferred-width [this] (layoutable/preferred-width (first children)))

    (preferred-height [this] 0))



(defn size-group-width [size-group]
  (apply max (conj (map (fn [element]
                          (:width (layoutable/preferred-size element
                                                             java.lang.Integer/MAX_VALUE
                                                             java.lang.Integer/MAX_VALUE)))
                        (:members @size-group) )
                   0)))

(defn size-group-height [size-group]
  (apply max (conj (map (fn [element]
                          (:height (layoutable/preferred-size element
                                                              java.lang.Integer/MAX_VALUE
                                                              java.lang.Integer/MAX_VALUE)))
                        (:members @size-group))
                   0)))

(layout/deflayout-not-memoized SizeGroupMember [size-group mode children]
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
                              (size-group-width size-group)
                              (:width child-size))
                     :height (if (#{:height :both} mode)
                               (size-group-height size-group)
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

(layout/deflayout-not-memoized Superimpose [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (vec (map-indexed (fn [index child]
                                     (assoc (layout/set-dimensions-and-layout child 0 0 requested-width requested-height)
                                       :z index))
                                   children))))

  (preferred-size [this available-width available-height]
                  (let [child-sizes (map (fn [child]
                                           (layoutable/preferred-size child java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE))
                                         children)]
                    {:width (apply max (map :width
                                            child-sizes))
                     :height (apply max (map :height
                                             child-sizes))})))

(layout/deflayout-with-state SizeDependent [preferred-size-function child-function]
  (layout [this state requested-width requested-height]
          (let [{:keys [state layoutable]} (gui/with-children state (child-function state requested-width requested-height))]
            (let [[state child-layout] (layout/set-dimensions-and-layout layoutable state
                                                                         0 0 requested-width requested-height)]
              [state
               (assoc this :children [child-layout])])))

  (preferred-size [this available-width available-height]
                  (preferred-size-function available-width available-height)))


(layout/deflayout-not-memoized Translate [translate-x translate-y child]

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

    (layout [dock-bottom requested-width requested-height  ]
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


(run-tests)

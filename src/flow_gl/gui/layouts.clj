(ns flow-gl.gui.layouts
  (:require  (flow-gl.graphics.command [translate :as translate]
                                       [scale :as scale]
                                       [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview])
             (flow-gl.gui [layout :as layout]
                          [quad-gui :as quad-gui]
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

#_(deflayout Absolute [children]

    (layout [absolute requested-width requested-height]
            (assoc absolute :layoutables
                   (vec (map #(layout/set-dimensions-and-layout % (or (:x %) 0) (or (:y %) 0) (layoutable/preferred-width %) (layoutable/preferred-height %))
                             children))))

    (preferred-width [absolute] (apply max (map (fn [layoutable]
                                                  (+ (:x layoutable)
                                                     (layoutable/preferred-width layoutable)))
                                                children)))

    (preferred-height [absolute] (apply max (map (fn [layoutable]
                                                   (+ (:y layoutable)
                                                      (layoutable/preferred-height layoutable)))
                                                 children))))

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
  (apply max (conj (map layoutable/preferred-width (:members @size-group))
                   0)))

(defn size-group-height [size-group]
  (apply max (conj (map layoutable/preferred-height (:members @size-group))
                   0)))

#_(layout/deflayout-not-memoized SizeGroupMember [size-group mode children]
    (layout [this requested-width requested-height]
            (assoc this :children
                   [(layout/set-dimensions-and-layout (first children)
                                                      0
                                                      0
                                                      (layoutable/preferred-width (first children))
                                                      (layoutable/preferred-height (first children)))]))


    (preferred-width [this] (if (#{:width :both} mode)
                              (size-group-width size-group)
                              (layoutable/preferred-width (first children))))
    (preferred-height [this] (if (#{:height :both} mode)
                               (size-group-height size-group)
                               (layoutable/preferred-height (first children)))))

#_(defn create-size-group []
    (atom {:members #{}}))

#_(defn size-group-member [size-group mode layoutable]
    (swap! size-group #(update-in % [:members] conj layoutable))
    (->SizeGroupMember size-group mode [layoutable]))



#_(defn grid [rows]
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
          (-> this
              (update-in [:children]
                         (fn [[under over]]
                           [(layout/set-dimensions-and-layout under 0 0 requested-width requested-height)
                            (assoc (layout/set-dimensions-and-layout over 0 0 requested-width requested-height)
                              :z 1)]))))

  (preferred-size [this available-width available-height]
                  (let [[under over] children
                        under-size (layoutable/preferred-size under available-width available-height)
                        over-size (layoutable/preferred-size over available-width available-height)]
                    {:width (max (:width under-size)
                                 (:width over-size))
                     :height (max (:height under-size)
                                  (:height over-size))})))

(layout/deflayout-with-state SizeDependent [preferred-size-function child-function]
  (layout [this state requested-width requested-height]
          (let [[state child-visual] (quad-gui/with-children state (child-function state requested-width requested-height))]
            (let [[state child-layout] (layout/set-dimensions-and-layout child-visual state
                                                                         0 0 requested-width requested-height)]
              [state
               (assoc this :children [child-layout])])))

  (preferred-size [this available-width available-height]
                  (preferred-size-function available-width available-height)))


#_(deflayout Translation [translate-x translate-y layoutable]

    (layout [translation requested-width requested-height  ]
            (assoc translation :layoutable
                   (layout/set-dimensions-and-layout layoutable
                                                     translate-x
                                                     translate-y
                                                     (layoutable/preferred-width layoutable)
                                                     (layoutable/preferred-height layoutable))))

    (preferred-width [translation] (+ translate-x (layoutable/preferred-width layoutable)))

    (preferred-height [translation] (+ translate-y (layoutable/preferred-height layoutable))))


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
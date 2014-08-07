(ns flow-gl.gui.layout
  (:require  (flow-gl.graphics.command [translate :as translate]
                                       [scale :as scale]
                                       [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview])
             (flow-gl.gui [layoutable :as layoutable]
                          [drawable :as drawable]))
  (:use clojure.test))

(defprotocol Layout
  (layout [layout requested-width requested-height]))


(extend Object
  Layout {:layout (fn [this requested-width requested-height] this)})

;; UTILITIES

(defn add-global-coordinates [layout global-x global-y]
  (-> (assoc layout
        :global-x global-x
        :global-y global-y)
      (update-in [:children]
                 (fn [children]
                   (for [child children]
                     (add-global-coordinates child
                                             (+ (:x child)
                                                global-x)

                                             (+ (:y child)
                                                global-y)))))))

(defn set-dimensions-and-layout [layout-instance x y width height]
  (-> layout-instance
      (layout width
              height)
      (assoc :x x
             :y y
             :width width
             :height height)))

(defn layout-drawing-commands [layout]
  (let [drawables (:children layout)]
    (vec (concat [(push-modelview/->PushModelview)]
                 (loop [commands []
                        x 0
                        y 0
                        drawables drawables]
                   (if (seq drawables)
                     (let [drawable (first drawables)]
                       (recur (concat commands
                                      (concat (if (or (not (= (:x drawable) x))
                                                      (not (= (:y drawable) y)))
                                                [(translate/->Translate (- (:x drawable)
                                                                           x)
                                                                        (- (:y drawable)
                                                                           y))]
                                                [])
                                              (drawable/drawing-commands drawable)))
                              (:x drawable)
                              (:y drawable)
                              (rest drawables)))
                     commands))
                 [(pop-modelview/->PopModelview)]))))


(defn draw-layout [layout graphics]
  (if-let [drawables (:children layout)]
    (let [old-transform (.getTransform graphics)]
      (loop [x 0
             y 0
             drawables drawables]
        (when (seq drawables)
          (let [drawable (first drawables)]
            (when (or (not (= (:x drawable) x))
                      (not (= (:y drawable) y)))
              (.translate graphics
                          (double (- (:x drawable)
                                     x))
                          (double (- (:y drawable)
                                     y))))
            (drawable/draw drawable graphics)
            (recur (:x drawable)
                   (:y drawable)
                   (rest drawables)))))
      (.setTransform graphics old-transform))))

(defn in-coordinates [x y layoutable]
  (and (>= x
           (:x layoutable))
       (<= x
           (+ (:x layoutable) (:width layoutable)))
       (>= y
           (:y layoutable))
       (<= y
           (+ (:y layoutable) (:height layoutable)))))

(defn filter-by-coordinates [x y layoutables]
  (filter (partial in-coordinates x y) layoutables))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

(defn children-in-coordinates [layout x y]
  (for [child-index (positions (partial in-coordinates x y) (:children layout))]
    (let [child (get-in layout [:children child-index])]
      (cons child-index (children-in-coordinates child
                                                 (-  x (:x child))
                                                 (-  y (:y child)))))))



#_(fact (children-in-coordinates {:x 0 :y 0 :width 100 :height 100
                                  :children [{:x 0 :y 0 :width 20 :height 30}
                                             {:x 10 :y 10 :width 10 :height 30
                                              :children [{:x 0 :y 0 :width 10 :height 10}
                                                         {:x 0 :y 10 :width 10 :height 10}]}]}
                                 15
                                 25)
        => '((0) (1 (1))))

(defn layout-index-path-to-layout-path [layout-index-path]
  (conj (interpose  :children layout-index-path) :children ))

#_(fact (layout-index-path-to-layout-path [0 0])
        => '(:children 0 :children 0))

(defn layout-index-paths-in-coordinates [layout parent-path x y z paths]
  (let [child-indexes (positions (fn [child] (or (in-coordinates x y child)
                                                 (:has-children-out-of-layout child)))
                                 (:children layout))]
    (if (empty? child-indexes)
      (if (in-coordinates (+  x (:x layout))
                          (+  y (:y layout))
                          layout)
        (conj paths {:path parent-path
                     :z z})
        paths)
      (loop [child-indexes child-indexes
             paths paths]
        (if-let [child-index (first child-indexes)]
          (let [child (get-in layout [:children child-index])]
            (recur (rest child-indexes)
                   (layout-index-paths-in-coordinates child
                                                      (vec (conj parent-path child-index))
                                                      (- x (:x child))
                                                      (- y (:y child))
                                                      (+ z (or (:z child) 0))
                                                      paths)))
          paths)))))

(deftest layout-index-paths-in-coordinates-test
  (is (= (layout-index-paths-in-coordinates {:x 0 :y 0 :width 20 :height 40
                                             :children [{:x 0 :y 0 :width 20 :height 40
                                                         :children [{:x 0 :y 0 :width 40 :height 40}
                                                                    {:x 0 :y 10 :width 40 :height 40}]}

                                                        {:x 10 :y 10 :width 10 :height 10
                                                         :has-children-out-of-layout true
                                                         :children [{:x 0 :y 0 :width 10 :height 10}
                                                                    {:x 0 :y 10 :z 1 :width 10 :height 10}]}

                                                        {:x 10 :y 20 :width 10 :height 10
                                                         :children [{:x 0 :y 0 :width 10 :height 10}
                                                                    {:x 0 :y 10 :z 1 :width 10 :height 10}]}]}
                                            []
                                            15
                                            25
                                            0
                                            [])
         [{:path [0 0], :z 0} {:path [0 1], :z 0} {:path [1 1], :z 1} {:path [2 0], :z 0}])))

(defn layout-paths-in-coordinates [layout x y]
  (map layout-index-path-to-layout-path
       (->> (layout-index-paths-in-coordinates layout [] x y 0 [])
            (sort-by :z)
            (map :path))))

(defn layout-index-paths-with-keyboard-event-handlers [layout parent-path]
  (if-let [child-indexes (seq (positions #(or (:handle-keyboard-event %) (:children %))
                                         (:children layout)))]
    (concat (if (:handle-keyboard-event layout)
              [parent-path]
              [])
            (mapcat (fn [child-index]
                      (let [child (get-in layout [:children child-index])]
                        (layout-index-paths-with-keyboard-event-handlers child
                                                                         (vec (conj parent-path child-index)))))
                    child-indexes))
    [parent-path]))

#_(fact (layout-index-paths-with-keyboard-event-handlers {:children [{:handle-keyboard-event :handler
                                                                      :children [{}
                                                                                 {:handle-keyboard-event :handler}]}

                                                                     {:children [{:handle-keyboard-event :handler}
                                                                                 {}]}]}
                                                         [])
        => '([0] [0 1] [1 0]))

(defn layout-paths-with-keyboard-event-handlers [layout]
  (map layout-index-path-to-layout-path
       (layout-index-paths-with-keyboard-event-handlers layout [])))

#_(defmacro deflayout [name parameters layout-implementation preferred-width-implementation preferred-height-implementation]
    (let [[layout-name layout-parameters & layout-body] layout-implementation
          [preferred-width-name preferred-width-parameters & preferred-width-body] preferred-width-implementation
          [preferred-height-name preferred-height-parameters & preferred-height-body] preferred-height-implementation]
      (assert (= layout-name 'layout) (str "invalid layout name" layout-name))
      (assert (= preferred-width-name 'preferred-width) (str "invalid preferred width name" preferred-width-name))
      (assert (= preferred-height-name 'preferred-height) (str "invalid preferred height name" preferred-height-name))

      `(do (defrecord ~name ~parameters
             Object
             (toString [this#] (layoutable/describe-layoutable this#)))

           (extend ~name
             Layout
             {:layout (memoize (fn ~layout-parameters (let [{:keys ~parameters} ~(first layout-parameters)] #_(println (str "running" ~name) #_~parameters) ~@layout-body)))}
             layoutable/Layoutable
             {:preferred-width (memoize (fn ~preferred-width-parameters (let [{:keys ~parameters} ~(first preferred-width-parameters)] ~@preferred-width-body) ))
              :preferred-height (memoize (fn ~preferred-height-parameters (let [{:keys ~parameters} ~(first preferred-height-parameters)] ~@preferred-height-body)))}))))

(defmacro deflayout [name parameters layout-implementation preferred-width-implementation preferred-height-implementation]
  (let [[layout-name layout-parameters & layout-body] layout-implementation
        [preferred-width-name preferred-width-parameters & preferred-width-body] preferred-width-implementation
        [preferred-height-name preferred-height-parameters & preferred-height-body] preferred-height-implementation]
    (assert (= layout-name 'layout) (str "invalid layout name" layout-name))
    (assert (= preferred-width-name 'preferred-width) (str "invalid preferred width name" preferred-width-name))
    (assert (= preferred-height-name 'preferred-height) (str "invalid preferred height name" preferred-height-name))

    `(defrecord ~name ~parameters
       Layout
       ~layout-implementation

       layoutable/Layoutable
       (layoutable/preferred-width ~preferred-width-parameters ~@preferred-width-body)
       (layoutable/preferred-height ~preferred-height-parameters ~@preferred-height-body)

       Object
       (toString [this#] (layoutable/describe-layoutable this#)))))


#_(defmacro deflayout-not-memoized [name parameters layout-implementation preferred-width-implementation preferred-height-implementation]
    (let [[layout-name layout-parameters & layout-body] layout-implementation
          [preferred-width-name preferred-width-parameters & preferred-width-body] preferred-width-implementation
          [preferred-height-name preferred-height-parameters & preferred-height-body] preferred-height-implementation]
      (assert (= layout-name 'layout) (str "invalid layout name" layout-name))
      (assert (= preferred-width-name 'preferred-width) (str "invalid preferred width name" preferred-width-name))
      (assert (= preferred-height-name 'preferred-height) (str "invalid preferred height name" preferred-height-name))

      `(defrecord ~name ~parameters
         Layout
         ~layout-implementation

         layoutable/Layoutable
         (layoutable/preferred-width ~preferred-width-parameters ~@preferred-width-body)
         (layoutable/preferred-height ~preferred-height-parameters ~@preferred-height-body)

         Object
         (toString [this#] (layoutable/describe-layoutable this#)))))

(defmacro deflayout-not-memoized [name parameters layout-implementation preferred-size-implementation]
  (let [[layout-name layout-parameters & layout-body] layout-implementation
        [preferred-size-name preferred-size-parameters & preferred-size-body] preferred-size-implementation]
    (assert (= layout-name 'layout) (str "invalid layout name" layout-name))
    (assert (= preferred-size-name 'preferred-size) (str "invalid preferred size name" preferred-size-name))

    `(defrecord ~name ~parameters
       Layout
       ~layout-implementation

       layoutable/Layoutable
       (layoutable/preferred-size ~preferred-size-parameters ~@preferred-size-body)

       Object
       (toString [this#] (layoutable/describe-layoutable this#)))))

(defn add-out-of-layout-hints [layout]
  (loop [children (:children layout)
         hinted-children []
         has-children-out-of-layout false]
    (if-let [child (first children)]
      (let [hinted-child (add-out-of-layout-hints child)]
        (recur (rest children)
               (conj hinted-children hinted-child)
               (or has-children-out-of-layout
                   (:has-children-out-of-layout hinted-child)
                   (:out-of-layout hinted-child))))
      (cond-> layout
              has-children-out-of-layout (assoc :has-children-out-of-layout true)
              (seq hinted-children) (assoc :children hinted-children)))))

(deftest add-out-of-layout-hints-test
  (is (= (add-out-of-layout-hints {:x 0 :y 0 :width 20 :height 40
                                   :children [{:x 0 :y 0 :width 20 :height 40
                                               :children [{:x 0 :y 0 :width 40 :height 40}
                                                          {:x 0 :y 10 :width 40 :height 40}]}

                                              {:x 10 :y 10 :width 10 :height 10
                                               :children [{:x 0 :y 0 :width 10 :height 10}
                                                          {:x 0 :y 10 :z 1 :width 10 :height 10 :out-of-layout true}]}]})
         {:x 0 :y 0 :width 20 :height 40 :has-children-out-of-layout true
          :children [{:x 0 :y 0 :width 20 :height 40
                      :children [{:x 0 :y 0 :width 40 :height 40}
                                 {:x 0 :y 10 :width 40 :height 40}]}

                     {:x 10 :y 10 :width 10 :height 10 :has-children-out-of-layout true
                      :children [{:x 0 :y 0  :width 10 :height 10}
                                 {:x 0 :y 10 :z 1 :width 10 :height 10 :out-of-layout true}]}]})))

;; LAYOUTS

(deflayout-not-memoized FixedSize [width height children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children 0]
                     #(set-dimensions-and-layout % 0 0 width height)))

  (preferred-size [this available-width available-height]
                  (let [child-size (layoutable/preferred-size (first children) width height)]
                    {:width (max width
                                 (:width child-size))

                     :height (max height
                                  (:height child-size))})))

(deflayout-not-memoized FloatRight [children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children]
                     (fn [[left right]]
                       (let [right-width (:width (layoutable/preferred-size right java.lang.Integer/MAX_VALUE requested-height))
                             left-width (- requested-width right-width)]
                         [(set-dimensions-and-layout left 0 0 left-width requested-height)
                          (set-dimensions-and-layout right left-width 0 right-width requested-height)]))))

  (preferred-size [this available-width available-height]
                  (let [[left right] children
                        right-size (layoutable/preferred-size right available-width available-height)
                        left-size (layoutable/preferred-size left (- available-width (:width right-size)) available-height)]
                    {:width (+ (:width left-size)
                               (:width right-size))

                     :height (max (:height left-size)
                                  (:height right-size))})))

(deflayout-not-memoized Box [margin children]
  (layout [box requested-width requested-height]
          (-> box
              (update-in [:children]
                         (fn [[outer inner]]
                           (let [inner-size (layoutable/preferred-size inner (- requested-width (* 2 margin)) (- requested-height (* 2 margin)))]
                             [(set-dimensions-and-layout outer 0 0 requested-width requested-height)
                              (assoc (set-dimensions-and-layout inner margin margin (:width inner-size) (:height inner-size))
                                :z 1)])))))

  (preferred-size [this available-width available-height]
                  (let [child-size (layoutable/preferred-size (second children) (- available-width (* 2 margin)) (- available-height (* 2 margin)))]
                    {:width (+ (* 2 margin)
                               (:width child-size))
                     :height (+ (* 2 margin)
                                (:height child-size))})))

(deflayout-not-memoized Preferred [child]
  (layout [this requested-width requested-height]
          (assoc this :children (let [preferred-size (layoutable/preferred-size child requested-width requested-height)]
                                  [(set-dimensions-and-layout child
                                                              0
                                                              0
                                                              (:width preferred-size)
                                                              (:height preferred-size))])))

  (preferred-size [this available-width available-height]
                  (layoutable/preferred-size child #_(first children) available-width available-height)))

(deflayout-not-memoized Margin [margin-top margin-right margin-bottom margin-left children]
  (layout [this requested-width requested-height]
          (update-in this [:children] (fn [[layoutable]]
                                        [(set-dimensions-and-layout layoutable
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

(deflayout Absolute [children]

  (layout [absolute requested-width requested-height]
          (assoc absolute :layoutables
                 (vec (map #(set-dimensions-and-layout % (or (:x %) 0) (or (:y %) 0) (layoutable/preferred-width %) (layoutable/preferred-height %))
                           children))))

  (preferred-width [absolute] (apply max (map (fn [layoutable]
                                                (+ (:x layoutable)
                                                   (layoutable/preferred-width layoutable)))
                                              children)))

  (preferred-height [absolute] (apply max (map (fn [layoutable]
                                                 (+ (:y layoutable)
                                                    (layoutable/preferred-height layoutable)))
                                               children))))

(deflayout-not-memoized VerticalStack [children]
  (layout [vertical-stack requested-width requested-height]
          (assoc vertical-stack :children
                 (loop [layouted-layoutables []
                        y 0
                        children children]
                   (if (seq children)
                     (let [height (:height (layoutable/preferred-size (first children) requested-width java.lang.Integer/MAX_VALUE))]
                       (recur (conj layouted-layoutables (set-dimensions-and-layout (first children)
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


(deflayout-not-memoized HorizontalStack [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (loop [layouted-layoutables []
                        x 0
                        children children]
                   (if (seq children)
                     (let [width (:width (layoutable/preferred-size (first children) java.lang.Integer/MAX_VALUE requested-height))]
                       (recur (conj layouted-layoutables (set-dimensions-and-layout (first children)
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
                     (set-dimensions-and-layout layoutable
                                                x
                                                y
                                                (:width preferred-size)
                                                height))))
      layouted-layoutables)))

(deflayout-not-memoized Flow [children]
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

(deflayout BottomOutOfLayout [children]
  (layout [this requested-width requested-height]
          (assoc this :children [(-> (set-dimensions-and-layout (first children)
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

#_(deflayout-not-memoized SizeGroupMember [size-group mode children]
    (layout [this requested-width requested-height]
            (assoc this :children
                   [(set-dimensions-and-layout (first children)
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


(deflayout Stack [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (vec (map #(set-dimensions-and-layout % 0 0 requested-width requested-height)
                           children))))

  (preferred-width [this]
                   (apply max (conj (map layoutable/preferred-width children)
                                    0)))

  (preferred-height [this]
                    (apply max (conj (map layoutable/preferred-height children)
                                     0))))

(deflayout-not-memoized Superimpose [children]
  (layout [this requested-width requested-height]
          (-> this
              (update-in [:children]
                         (fn [[under over]]
                           [(set-dimensions-and-layout under 0 0 requested-width requested-height)
                            (assoc (set-dimensions-and-layout over 0 0 requested-width requested-height)
                              :z 1)]))))

  (preferred-size [this available-width available-height]
                  (let [[under over] children
                        under-size (layoutable/preferred-size under available-width available-height)
                        over-size (layoutable/preferred-size over available-width available-height)]
                    {:width (max (:width under-size)
                                 (:width over-size))
                     :height (max (:height under-size)
                                  (:height over-size))})))

(deflayout-not-memoized SizeDependent [preferred-size-function child-function]
  (layout [this requested-width requested-height]
          (assoc this :children [(set-dimensions-and-layout (child-function requested-width requested-height)
                                                            0 0 requested-width requested-height)]))

  (preferred-size [this available-width available-height]
                  (preferred-size-function available-width available-height)))


(deflayout Translation [translate-x translate-y layoutable]

  (layout [translation requested-width requested-height  ]
          (assoc translation :layoutable
                 (set-dimensions-and-layout layoutable
                                            translate-x
                                            translate-y
                                            (layoutable/preferred-width layoutable)
                                            (layoutable/preferred-height layoutable))))

  (preferred-width [translation] (+ translate-x (layoutable/preferred-width layoutable)))

  (preferred-height [translation] (+ translate-y (layoutable/preferred-height layoutable))))


(deflayout DockBottom [layoutable]

  (layout [dock-bottom requested-width requested-height  ]
          (let [height (layoutable/preferred-height layoutable )]
            (assoc dock-bottom :layoutable
                   (set-dimensions-and-layout layoutable
                                              0
                                              (- requested-height
                                                 height)
                                              (layoutable/preferred-width layoutable)
                                              height))))

  (preferred-width [dock-bottom] (layoutable/preferred-width layoutable))

  (preferred-height [dock-bottom] (layoutable/preferred-height layoutable)))


(run-tests)

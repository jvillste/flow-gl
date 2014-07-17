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
      (assoc :x x
             :y y
             :width width
             :height height)
      (layout width
              height)))

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
                          (+  y (:y layout)) layout)
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


(defmacro deflayout-not-memoized [name parameters layout-implementation preferred-width-implementation preferred-height-implementation]
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

(deflayout FixedSize [width height children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children 0]
                     #(set-dimensions-and-layout % 0 0 width height)))

  (preferred-width [this] width)

  (preferred-height [this] height))

(deflayout Box [margin children]
  (layout [box requested-width requested-height]
          (-> box
              (update-in [:children]
                         (fn [[outer inner]]
                           [(set-dimensions-and-layout outer 0 0 requested-width requested-height)
                            (assoc (set-dimensions-and-layout inner margin margin (layoutable/preferred-width inner) (layoutable/preferred-height inner))
                              :z 1)]))))

  (preferred-width [box] (+ (* 2 margin)
                            (layoutable/preferred-width (second children))))

  (preferred-height [box] (+ (* 2 margin)
                             (layoutable/preferred-height (second children)))))

(deflayout-not-memoized Margin [margin-top margin-right margin-bottom margin-left children]
  (layout [this requested-width requested-height]
          (update-in this [:children] (fn [[layoutable]]
                                        [(set-dimensions-and-layout layoutable
                                                                    margin-left
                                                                    margin-top
                                                                    (layoutable/preferred-width layoutable)
                                                                    (layoutable/preferred-height layoutable))])))

  (preferred-width [this] (+ margin-left margin-right
                             (layoutable/preferred-width (first children))))

  (preferred-height [this] (+ margin-top margin-bottom
                              (layoutable/preferred-height (first children)))))

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
                 (let [width (apply max (conj (map layoutable/preferred-width children)
                                              0))]
                   (loop [layouted-layoutables []
                          y 0
                          children children]
                     (if (seq children)
                       (let [height (layoutable/preferred-height (first children))]
                         (recur (conj layouted-layoutables (set-dimensions-and-layout (first children) 0 y width height))
                                (+ y height)
                                (rest children)))
                       layouted-layoutables)))))

  (preferred-width [vertical-stack] (apply max (conj (map layoutable/preferred-width children)
                                                     0)))

  (preferred-height [vertical-stack] (reduce + (map layoutable/preferred-height children))))


(deflayout HorizontalStack [children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 (let [height (apply max (conj (map layoutable/preferred-height children)
                                               0))]
                   (loop [layouted-layoutables []
                          x 0
                          children children]
                     (if (seq children)
                       (let [width (layoutable/preferred-width (first children))]
                         (recur (conj layouted-layoutables (set-dimensions-and-layout (first children) x 0 width height))
                                (+ x width)
                                (rest children)))
                       layouted-layoutables)))))

  (preferred-width [this] (reduce + (map layoutable/preferred-width children)))

  (preferred-height [this] (apply max (conj (map layoutable/preferred-height children)
                                            0))))

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

(deflayout-not-memoized SizeGroupMember [size-group mode children]
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

(deflayout Superimpose [layoutables]

  (layout [this requested-width requested-height  ]
          (assoc this :layoutables
                 (vec (map #(set-dimensions-and-layout % 0 0 requested-width requested-height)
                           layoutables))))

  (preferred-width [this]
                   (layoutable/preferred-width (first layoutables)))

  (preferred-height [this]
                    (layoutable/preferred-height (first layoutables))))

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

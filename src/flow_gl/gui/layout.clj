(ns flow-gl.gui.layout
  (:require  (flow-gl.graphics.command [translate :as translate]
                                       [scale :as scale]
                                       [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview])
             (flow-gl.gui [layoutable :as layoutable]
                          [drawable :as drawable]))
  (:use midje.sweet))

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



(fact (children-in-coordinates {:x 0 :y 0 :width 100 :height 100
                                :children [{:x 0 :y 0 :width 20 :height 30}
                                           {:x 10 :y 10 :width 10 :height 30
                                            :children [{:x 0 :y 0 :width 10 :height 10}
                                                       {:x 0 :y 10 :width 10 :height 10}]}]}
                               15
                               25)
      => '((0) (1 (1))))

(defn layout-index-path-to-layout-path [layout-index-path]
  (conj (interpose  :children layout-index-path) :children ))

(fact (layout-index-path-to-layout-path [0 0])
      => '(:children 0 :children 0))

(defn layout-index-paths-in-coordinates [layout parent-path x y]
  (if-let [child-indexes (seq (positions (partial in-coordinates x y) (:children layout)))]
    (mapcat (fn [child-index]
              (let [child (get-in layout [:children child-index])]
                (layout-index-paths-in-coordinates child
                                                   (vec (conj parent-path child-index))
                                                   (-  x (:x child))
                                                   (-  y (:y child)))))
            child-indexes)
    [parent-path]))

(fact (layout-index-paths-in-coordinates {:children [{:x 0 :y 0 :width 20 :height 40
                                                      :children [{:x 0 :y 0 :width 40 :height 40}
                                                                 {:x 0 :y 10 :width 40 :height 40}]}

                                                     {:x 10 :y 10 :width 10 :height 30
                                                      :children [{:x 0 :y 0 :width 10 :height 10}
                                                                 {:x 0 :y 10 :width 10 :height 10}]}]}
                                         []
                                         15
                                         25)
      => '([0 0] [0 1] [1 1]))

(defn layout-paths-in-coordinates [layout x y]
  (map layout-index-path-to-layout-path
       (layout-index-paths-in-coordinates layout [] x y)))

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

(fact (layout-index-paths-with-keyboard-event-handlers {:children [{:handle-keyboard-event :handler
                                                                    :children [{}
                                                                               {:handle-keyboard-event :handler}]}

                                                                   {:children [{:handle-keyboard-event :handler}
                                                                               {}]}]}
                                                       [])
      => '([0 0] [0 1] [1 1]))

(defn layout-paths-with-keyboard-event-handlers [layout]
  (map layout-index-path-to-layout-path
       (layout-index-paths-with-keyboard-event-handlers layout [])))

(defmacro deflayout [name parameters layout-implementation preferred-width-implementation preferred-height-implementation]
  `(defrecord ~name ~parameters
     Layout
     ~layout-implementation

     layoutable/Layoutable
     ~preferred-width-implementation

     ~preferred-height-implementation

     drawable/Drawable
     (drawing-commands [this#] (layout-drawing-commands this#))

     drawable/Java2DDrawable
     (draw [this# graphics#] (draw-layout this# graphics#))

     Object
     (toString [this#] (layoutable/describe-layoutable this#))))

;; LAYOUTS

(deflayout FixedSize [width height children]
  (layout [this requested-width requested-height]
          (update-in this
                     [:children 0]
                     #(set-dimensions-and-layout % 0 0 width height)))

  (layoutable/preferred-width [this] width)

  (layoutable/preferred-height [this] height))

(deflayout Box [margin children]
  (layout [box requested-width requested-height]
          (-> box
              (update-in [:children]
                         (fn [[outer inner]]
                           [(set-dimensions-and-layout outer 0 0 requested-width requested-height)
                            (set-dimensions-and-layout inner margin margin (layoutable/preferred-width inner) (layoutable/preferred-height inner))]))))

  (layoutable/preferred-width [box] (+ (* 2 margin)
                                       (layoutable/preferred-width (second children))))

  (layoutable/preferred-height [box] (+ (* 2 margin)
                                        (layoutable/preferred-height (second children)))))

(deflayout Margin [margin-top margin-right margin-bottom margin-left children]
  (layout [this requested-width requested-height]
          (update-in this [:children] (fn [[layoutable]]
                                        [(set-dimensions-and-layout layoutable
                                                                    margin-left
                                                                    margin-top
                                                                    (layoutable/preferred-width layoutable)
                                                                    (layoutable/preferred-height layoutable))])))

  (layoutable/preferred-width [this] (+ margin-left margin-right
                                        (layoutable/preferred-width (first children))))

  (layoutable/preferred-height [this] (+ margin-top margin-bottom
                                         (layoutable/preferred-height (first children)))))

(deflayout Absolute [layoutables]

  (layout [absolute requested-width requested-height]
          (assoc absolute :layoutables
                 (vec (map #(set-dimensions-and-layout % (or (:x %) 0) (or (:y %) 0) (layoutable/preferred-width %) (layoutable/preferred-height %))
                           layoutables))))

  (layoutable/preferred-width [absolute] (apply max (map (fn [layoutable]
                                                           (+ (:x layoutable)
                                                              (layoutable/preferred-width layoutable)))
                                                         layoutables)))

  (layoutable/preferred-height [absolute] (apply max (map (fn [layoutable]
                                                            (+ (:y layoutable)
                                                               (layoutable/preferred-height layoutable)))
                                                          layoutables))))

(deflayout VerticalStack [children]
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

  (layoutable/preferred-height [vertical-stack] (reduce + (map layoutable/preferred-height children)))

  (layoutable/preferred-width [vertical-stack] (apply max (conj (map layoutable/preferred-width children)
                                                                0))))

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

  (layoutable/preferred-height [this] (apply max (conj (map layoutable/preferred-height children)
                                                       0)))

  (layoutable/preferred-width [this] (reduce + (map layoutable/preferred-width children))))


(defn size-group-width [size-group]
  (apply max (conj (map layoutable/preferred-width (:members @size-group))
                   0)))

(defn size-group-height [size-group]
  (apply max (conj (map layoutable/preferred-height (:members @size-group))
                   0)))

(deflayout SizeGroupMember [size-group mode children]
  (layout [this requested-width requested-height]
          (assoc this :children
                 [(set-dimensions-and-layout (first children)
                                             0
                                             0
                                             (layoutable/preferred-width (first children))
                                             (layoutable/preferred-height (first children)))]))


  (layoutable/preferred-height [this] (if (#{:height :both} mode)
                                        (size-group-height size-group)
                                        (layoutable/preferred-height (first children))))

  (layoutable/preferred-width [this] (if (#{:width :both} mode)
                                       (size-group-width size-group)
                                       (layoutable/preferred-width (first children)))))

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

  (layoutable/preferred-width [this]
                              (apply max (conj (map layoutable/preferred-width children)
                                               0)))

  (layoutable/preferred-height [this]
                               (apply max (conj (map layoutable/preferred-height children)
                                                0))))

(deflayout Superimpose [layoutables]

  (layout [this requested-width requested-height  ]
          (assoc this :layoutables
                 (vec (map #(set-dimensions-and-layout % 0 0 requested-width requested-height)
                           layoutables))))

  (layoutable/preferred-width [this]
                              (layoutable/preferred-width (first layoutables)))

  (layoutable/preferred-height [this]
                               (layoutable/preferred-height (first layoutables))))

(deflayout Translation [translate-x translate-y layoutable]

  (layout [translation requested-width requested-height  ]
          (assoc translation :layoutable
                 (set-dimensions-and-layout layoutable
                                            translate-x
                                            translate-y
                                            (layoutable/preferred-width layoutable)
                                            (layoutable/preferred-height layoutable))))

  (layoutable/preferred-width [translation] (+ translate-x (layoutable/preferred-width layoutable)))

  (layoutable/preferred-height [translation] (+ translate-y (layoutable/preferred-height layoutable))))


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

  (layoutable/preferred-width [dock-bottom] (layoutable/preferred-width layoutable))

  (layoutable/preferred-height [dock-bottom] (layoutable/preferred-height layoutable)))

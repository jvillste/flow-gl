(ns flow-gl.gui.layout
  (:require  (flow-gl.graphics.command [translate :as translate]
                                       [scale :as scale]
                                       [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview])
             (flow-gl.gui [layoutable :as layoutable]
                          [drawable :as drawable])))

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

(defn children-in-coordinates [layout view-state x y]
  (let [children (filter-by-coordinates x y (:children layout))]
    (concat children (mapcat (fn [child] (children-in-coordinates child
                                                                  view-state
                                                                  (+ x (:x child))
                                                                  (+ y (:y child))))
                             children))))

(defmacro deflayout [name parameters layout-implementation preferred-width-implementation preferred-height-implementation]
  `(defrecord ~name ~parameters
     Layout
     ~layout-implementation

     layoutable/Layoutable
     ~preferred-width-implementation

     ~preferred-height-implementation

     drawable/Drawable
     (drawing-commands [this#] (layout-drawing-commands this#))

     Object
     (toString [this#] (layoutable/describe-layoutable this# (str ~name)))))

;; LAYOUTS

(deflayout Box [margin children]
  (layout [box requested-width requested-height  ]
          (-> box
              (update-in [:children]
                         (fn [[outer inner]]
                           [(set-dimensions-and-layout outer 0 0 requested-width requested-height)
                            (set-dimensions-and-layout inner margin margin    (layoutable/preferred-width inner) (layoutable/preferred-height inner))]))))

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

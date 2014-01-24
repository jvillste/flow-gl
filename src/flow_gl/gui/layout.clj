(ns flow-gl.gui.layout
  (:require  (flow-gl.graphics.command [translate :as translate]
                                       [scale :as scale]
                                       [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview])
             (flow-gl.gui [layoutable :as layoutable]
                          [drawable :as drawable])))

(defprotocol Layout
  (layout [layout requested-width requested-height globa-x global-y])
  (children [layout]))


(extend Object
  Layout {:layout (fn [this requested-width requested-height global-x global-y] this)
          :children (fn [this] [])})


;; UTILITIES

(defn set-dimensions-and-layout [layout-instance x y global-x global-y width height]
  (-> layout-instance
      (assoc :x x
             :y y
             :global-x (+ x global-x)
             :global-y (+ y global-y)
             :width width
             :height height)
      (layout width
              height
              (+ x global-x)
              (+ y global-y))))

(defn layout-drawing-commands [layout]
  (let [drawables (children layout)]
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
  (let [children (filter-by-coordinates x y (children layout))]
    (concat children (mapcat (fn [child] (children-in-coordinates child
                                                                  view-state
                                                                  (+ x (:x child))
                                                                  (+ y (:y child))))
                             children))))

;; LAYOUTS

(defrecord Box [margin outer inner]
  Layout
  (layout [box requested-width requested-height global-x global-y]
    (-> box
        (update-in [:outer] set-dimensions-and-layout 0 0  global-x global-y requested-width requested-height)
        (update-in [:inner] set-dimensions-and-layout margin margin  global-x global-y (layoutable/preferred-width inner) (layoutable/preferred-height inner))))

  (children [this] [outer inner])

  layoutable/Layoutable
  (layoutable/preferred-width [box] (+ (* 2 margin)
                                       (layoutable/preferred-width inner)))

  (layoutable/preferred-height [box] (+ (* 2 margin)
                                        (layoutable/preferred-height inner)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))


  Object
  (toString [this] (layoutable/describe-layoutable this "Box" :margin :outer :inner)))


(defrecord Margin [margin-top margin-right margin-bottom margin-left layoutable]
  Layout
  (layout [this requested-width requested-height global-x global-y]
    (update-in this [:layoutable] set-dimensions-and-layout
               margin-left
               margin-top
               global-x
               global-y
               (layoutable/preferred-width layoutable)
               (layoutable/preferred-height layoutable)))

  (children [this] [layoutable])

  layoutable/Layoutable
  (layoutable/preferred-width [this] (+ margin-left margin-right
                                        (layoutable/preferred-width layoutable)))

  (layoutable/preferred-height [this] (+ margin-top margin-bottom
                                         (layoutable/preferred-height layoutable)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Margin" :margin-left :margin-top :margin-right :margin-bottom :layoutable)))



(defrecord Absolute [layoutables]
  Layout
  (layout [absolute requested-width requested-height global-x global-y]
    (assoc absolute :layoutables
           (vec (map #(set-dimensions-and-layout % (or (:x %) 0) (or (:y %) 0) global-x global-y (layoutable/preferred-width %) (layoutable/preferred-height %))
                     layoutables))))

  (children [this] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-width [absolute] (apply max (map (fn [layoutable]
                                                           (+ (:x layoutable)
                                                              (layoutable/preferred-width layoutable)))
                                                         layoutables)))

  (layoutable/preferred-height [absolute] (apply max (map (fn [layoutable]
                                                            (+ (:y layoutable)
                                                               (layoutable/preferred-height layoutable)))
                                                          layoutables)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Absolute" :layoutables)))



(defrecord VerticalStack [layoutables]
  Layout
  (layout [vertical-stack requested-width requested-height global-x global-y]
    (assoc vertical-stack :layoutables
           (let [width (apply max (conj (map layoutable/preferred-width layoutables)
                                        0))]
             (loop [layouted-layoutables []
                    y 0
                    layoutables layoutables]
               (if (seq layoutables)
                 (let [height (layoutable/preferred-height (first layoutables))]
                   (recur (conj layouted-layoutables (set-dimensions-and-layout (first layoutables) 0 y  global-x global-y width height))
                          (+ y height)
                          (rest layoutables)))
                 layouted-layoutables)))))

  (children [this] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-height [vertical-stack] (reduce + (map layoutable/preferred-height layoutables)))

  (layoutable/preferred-width [vertical-stack] (apply max (conj (map layoutable/preferred-width layoutables)
                                                                0)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "VerticalStack" :layoutables)))

(defrecord HorizontalStack [layoutables]
  Layout
  (layout [this requested-width requested-height global-x global-y]
    (assoc this :layoutables
           (let [height (apply max (conj (map layoutable/preferred-height layoutables)
                                         0))]
             (loop [layouted-layoutables []
                    x 0
                    layoutables layoutables]
               (if (seq layoutables)
                 (let [width (layoutable/preferred-width (first layoutables))]
                   (recur (conj layouted-layoutables (set-dimensions-and-layout (first layoutables) x 0  global-x global-y width height))
                          (+ x width)
                          (rest layoutables)))
                 layouted-layoutables)))))

  (children [this] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-height [this] (apply max (conj (map layoutable/preferred-height layoutables)
                                                       0)))

  (layoutable/preferred-width [this] (reduce + (map layoutable/preferred-width layoutables)))


  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))


  Object
  (toString [this] (layoutable/describe-layoutable this "HorizontalStack" :layoutables)))



(defn size-group-width [size-group]
  (apply max (conj (map layoutable/preferred-width (:members @size-group))
                   0)))

(defn size-group-height [size-group]
  (apply max (conj (map layoutable/preferred-height (:members @size-group))
                   0)))


(defrecord SizeGroupMember [size-group mode layoutable]
  Layout
  (layout [this requested-width requested-height global-x global-y]
    (assoc this :layoutable
           (set-dimensions-and-layout layoutable
                                      0
                                      0
                                      global-x
                                      global-y
                                      (layoutable/preferred-width layoutable)
                                      (layoutable/preferred-height layoutable))))

  (children [this] [layoutable])

  layoutable/Layoutable
  (layoutable/preferred-height [this] (if (#{:height :both} mode)
                                        (size-group-height size-group)
                                        (layoutable/preferred-height layoutable)))

  (layoutable/preferred-width [this] (if (#{:width :both} mode)
                                       (size-group-width size-group)
                                       (layoutable/preferred-width layoutable)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "SizeGroupMember" :size-group :layoutables)))

(defn create-size-group []
  (atom {:members #{}}))

(defn size-group-member [size-group mode layoutable]
  (swap! size-group #(update-in % [:members] conj layoutable))
  (->SizeGroupMember size-group mode layoutable))



(defn grid [rows]
  (let [size-groups (repeatedly create-size-group)]
    (->VerticalStack
     (for [row rows]
       (->HorizontalStack (for [[cell size-group] (partition 2
                                                             (interleave row
                                                                         size-groups))]
                            (size-group-member size-group :width cell)))))))




(defrecord Stack [layoutables]
  Layout
  (layout [this requested-width requested-height global-x global-y]
    (assoc this :layoutables
           (vec (map #(set-dimensions-and-layout % 0 0  global-x global-y requested-width requested-height)
                     layoutables))))

  (children [this] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-width [this]
    (apply max (conj (map layoutable/preferred-width layoutables)
                     0)))

  (layoutable/preferred-height [this]
    (apply max (conj (map layoutable/preferred-height layoutables)
                     0)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Stack" :layoutables)))

(defrecord Superimpose [layoutables]
  Layout
  (layout [this requested-width requested-height global-x global-y]
    (assoc this :layoutables
           (vec (map #(set-dimensions-and-layout % 0 0  global-x global-y requested-width requested-height)
                     layoutables))))

  (children [this] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-width [this]
    (layoutable/preferred-width (first layoutables)))

  (layoutable/preferred-height [this]
    (layoutable/preferred-height (first layoutables)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Superimpose" :layoutables)))


(defrecord Translation [translate-x translate-y layoutable]
  Layout
  (layout [translation requested-width requested-height global-x global-y]
    (assoc translation :layoutable
           (set-dimensions-and-layout layoutable
                                      translate-x
                                      translate-y
                                      global-x
                                      global-y
                                      (layoutable/preferred-width layoutable)
                                      (layoutable/preferred-height layoutable))))

  (children [this] [layoutable])

  layoutable/Layoutable
  (layoutable/preferred-width [translation] (+ translate-x (layoutable/preferred-width layoutable)))

  (layoutable/preferred-height [translation] (+ translate-y (layoutable/preferred-height layoutable)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Translation" :translate-x :translate-y :layoutable)))

(defrecord DockBottom [layoutable]
  Layout
  (layout [dock-bottom requested-width requested-height global-x global-y]
    (let [height (layoutable/preferred-height layoutable )]
      (assoc dock-bottom :layoutable
             (set-dimensions-and-layout layoutable
                                        0
                                        (- requested-height
                                           height)
                                        global-x
                                        global-y
                                        (layoutable/preferred-width layoutable)
                                        height))))
  (children [this] [layoutable])

  layoutable/Layoutable
  (layoutable/preferred-width [dock-bottom] (layoutable/preferred-width layoutable))

  (layoutable/preferred-height [dock-bottom] (layoutable/preferred-height layoutable))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "DockBottom" :layoutable)))

(ns flow-gl.gui.drawable
  (:require  (flow-gl.graphics.command [text :as text]
                                       [image :as image]
                                       [triangle-batch :as triangle-batch])
             (flow-gl.graphics [font :as font]
                               [vector :as vector]
                               [text :as graphics-text]
                               [buffered-image :as buffered-image])
             (flow-gl.gui [layoutable :as layoutable]))
  (:import [java.awt.geom Rectangle2D$Float RoundRectangle2D$Float]
           [java.awt Color RenderingHints]))

(defprotocol Drawable
  (drawing-commands [drawable]))

(defprotocol Java2DDrawable
  (draw [this graphics]))

;; DRAWABLES

(defrecord Text [contents font color]
  Drawable
  (drawing-commands [text]
    [(text/create contents
                  font
                  color)])

  Java2DDrawable
  (draw [this graphics]
    (graphics-text/draw graphics color font contents))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    {:width (font/width font contents)
     :height (font/height font)})

  Object
  (toString [this] (layoutable/describe-layoutable this)))

(defrecord Image [buffered-image]
  Java2DDrawable
  (draw [this graphics]
    (doto graphics
      (.drawImage buffered-image (int 0) (int 0) nil)))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    {:width (.getWidth buffered-image)
     :height (.getHeight buffered-image)})
  (preferred-width [this] 100)
  (preferred-height [this] 100)

  Object
  (toString [this] (layoutable/describe-layoutable this "Image" :file-name)))

(defrecord Empty [width height]
  Drawable
  (drawing-commands [empty] [])

  layoutable/Layoutable
  (preferred-size [this available-width available-height] {:width width
                                                           :height height})
  (preferred-width [empty] width)
  (preferred-height [empty] height)

  Java2DDrawable
  (draw [this graphics])

  Object
  (toString [this] (layoutable/describe-layoutable this "Empty" :width :height)))

(defrecord Rectangle [width height color]
  Drawable
  (drawing-commands [rectangle]
    [(vector/rectangle 0
                       0
                       width
                       height
                       color)])

  layoutable/Layoutable
  (preferred-size [this available-width available-height] {:width width
                                                           :height height})

  Java2DDrawable
  (draw [this graphics]
    (let [[r g b a] (map float color)]
      (doto graphics
        (.setColor (Color. r g b a))
        (.fill (Rectangle2D$Float. 0 0 width height)))))

  Object
  (toString [this] (layoutable/describe-layoutable this)))

(defrecord Triangle [color x1 y1 x2 y2 x3 y3]
  Drawable
  (drawing-commands [rectangle]
    (vector/triangle color x1 y1 x2 y2 x3 y3))

  layoutable/Layoutable
  (preferred-width [rectangle] (max x1 x2 x3))
  (preferred-height [rectangle] (max y1 y2 y3))

  Object
  (toString [this] (layoutable/describe-layoutable this "Triangle" :color :x1 :y1 :x2 :y2 :x3 :y3)))


(defn single-color [coordinates color]
  (apply concat (-> (count coordinates)
                    (/ 2)
                    (repeat color))))

(defn single-color-triangle-batch [coordinates color]
  (triangle-batch/create coordinates
                         (single-color coordinates color)))

(defn single-color-triangle-fan [coordinates color]
  (triangle-batch/create coordinates
                         (single-color coordinates color)))

(defn line-coordinates [width x1 y1 x2 y2]
  (let [line-angle (Math/atan2 (- x2 x1)
                               (- y2 y1))
        cap-angle (- Math/PI
                     line-angle)

        half-width (/ width 2)

        cap-x (* (Math/cos cap-angle)
                 half-width)
        cap-y (* (Math/sin cap-angle)
                 half-width)

        v1x (- x1 cap-x)
        v1y (- y1 cap-y)
        v2x (- x2 cap-x)
        v2y (- y2 cap-y)
        v3x (+ x2 cap-x)
        v3y (+ y2 cap-y)
        v4x (+ x1 cap-x)
        v4y (+ y1 cap-y)]

    [v1x v1y
     v4x v4y
     v2x v2y

     v4x v4y
     v2x v2y
     v3x v3y]))

(defrecord Line [color line-width x1 y1 x2 y2]
  Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (line-coordinates line-width x1 y1 x2 y2)
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] (max x1 x2))
  (preferred-height [this] (max y1 y2))

  Object
  (toString [this] (layoutable/describe-layoutable this "Line" :color :x1 :y1 :x2 :y2)))

(defn polyline-coordinates [width coordinates]
  (let [joint-angles (concat [(let [[x1 y1 x2 y2] (take 4 coordinates)]
                                (- Math/PI
                                   (Math/atan2 (- x2 x1)
                                               (- y2 y1))))]

                             (for [[x1 y1 x2 y2 x3 y3] (partition 6 2 coordinates)]
                               #_(Math/atan2 (- y2 y1)
                                             (- x2 x1))
                               (+ (/ Math/PI 2)
                                  (/ (+ (Math/atan2 (- y2 y1)
                                                    (- x2 x1))
                                        (Math/atan2 (- y3 y2)
                                                    (- x3 x2)))
                                     2)))

                             [(let [[x1 y1 x2 y2] (take-last 4 coordinates)]
                                (- Math/PI
                                   (Math/atan2 (- x2 x1)
                                               (- y2 y1))))])]

    (apply concat (for [[[joint-angle-1 joint-angle-2] [x1 y1 x2 y2]] (partition 2 (interleave (partition 2 1 joint-angles)
                                                                                               (partition 4 2 coordinates)))]
                    (let [half-width (/ width 2)

                          joint-x-1 (* (Math/cos joint-angle-1)
                                       half-width)
                          joint-y-1 (* (Math/sin joint-angle-1)
                                       half-width)

                          joint-x-2 (* (Math/cos joint-angle-2)
                                       half-width)
                          joint-y-2 (* (Math/sin joint-angle-2)
                                       half-width)

                          v1x (- x1 joint-x-1)
                          v1y (- y1 joint-y-1)
                          v2x (- x2 joint-x-2)
                          v2y (- y2 joint-y-2)
                          v3x (+ x2 joint-x-2)
                          v3y (+ y2 joint-y-2)
                          v4x (+ x1 joint-x-1)
                          v4y (+ y1 joint-y-1)]

                      [v1x v1y
                       v3x v3y
                       v2x v2y

                       v1x v1y
                       v4x v4y
                       v3x v3y])))))

(defrecord Polyline [color line-width coordinates]
  Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (polyline-coordinates line-width coordinates)
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] (apply max (apply concat (partition 1 2 coordinates))))
  (preferred-height [this] (apply max (apply concat (partition 1 2 (rest coordinates)))))

  Object
  (toString [this] (layoutable/describe-layoutable this "Polyline" :color :line-width :coordinates)))

(defn arc-vertices [from-angle to-angle radius]
  (let [angle-difference (- to-angle from-angle)
        n (int (* 30
                  (/ angle-difference
                     (* 2 Math/PI))))
        cos-sin (for [i (range 0 (+ n 1))]
                  (let [angle (-> (* i angle-difference)
                                  (/ n)
                                  (+ from-angle))]
                    [(Math/cos angle)
                     (Math/sin angle)]))]
    (map (fn [[cos sin]]
           [(* radius cos)
            (* radius sin)] )
         cos-sin)))

(defn circle-coordinates [segment]
  (let [n 20
        cos-sin (for [i (range n)]
                  (let [angle (-> (* i 2 Math/PI)
                                  (/ n))]
                    [(Math/cos angle)
                     (Math/sin angle)]))]
    (apply concat

           (let [[cos2 sin2] (first cos-sin)
                 [cos1 sin1] (last cos-sin)]
             (segment cos1 sin1 cos2 sin2))

           (for [[[cos1 sin1] [cos2 sin2]] (partition 2 1 cos-sin)]
             (segment cos1 sin1 cos2 sin2)))))

(defrecord FilledCircle [color radius]
  Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (circle-coordinates (fn [cos1 sin1 cos2 sin2]
                                                        [radius radius
                                                         (+ radius (* radius cos2)) (+ radius (* radius sin2))
                                                         (+ radius (* radius cos1)) (+ radius (* radius sin1))]))
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] (* 2 radius))
  (preferred-height [this] (* 2 radius))

  Object
  (toString [this] (layoutable/describe-layoutable this "FilledCircle" :color :radius)))

(defrecord Circle [color radius line-width]
  Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (circle-coordinates (fn [cos1 sin1 cos2 sin2]
                                                        (let [inner-radius (- radius line-width)

                                                              x1 (* inner-radius
                                                                    cos1)
                                                              y1 (* inner-radius
                                                                    sin1)

                                                              x2 (* radius
                                                                    cos1)
                                                              y2 (* radius
                                                                    sin1)

                                                              x3 (* radius
                                                                    cos2)
                                                              y3 (* radius
                                                                    sin2)

                                                              x4 (* inner-radius
                                                                    cos2)
                                                              y4 (* inner-radius
                                                                    sin2)]
                                                          [x1 y1
                                                           x3 y3
                                                           x2 y2

                                                           x1 y1
                                                           x4 y4
                                                           x3 y3])))
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] (* 2 radius))
  (preferred-height [this] (* 2 radius))

  Object
  (toString [this] (layoutable/describe-layoutable this "Circle" :color :radius :line-width)))

(defn rounded-rectangle-vertices [width height radius]
  (apply concat (concat (map (fn [[x y]] [(+ x radius) (+ y radius)]) (arc-vertices Math/PI
                                                                                    (* (/ 3 2) Math/PI)
                                                                                    radius))
                        (map (fn [[x y]] [(+ x (- width radius)) (+ y radius)]) (arc-vertices (* (/ 3 2) Math/PI)
                                                                                              (* 2 Math/PI)
                                                                                              radius))
                        (map (fn [[x y]] [(+ x (- width radius)) (+ y (- height radius))]) (arc-vertices 0
                                                                                                         (/ Math/PI 2)
                                                                                                         radius))
                        (map (fn [[x y]] [(+ x radius) (+ y (- height radius))]) (arc-vertices (/ Math/PI 2)
                                                                                               Math/PI
                                                                                               radius)))))

(defn triangle-fan [vertices]
  (let [root-x (first vertices)
        root-y (second vertices)]
    (apply concat (for [[x1 y1 x2 y2] (partition 4 2 (drop 2 vertices))]
                    [root-x root-y
                     x2 y2
                     x1 y1]))))

(defrecord FilledRoundedRectangle [width height radius color]
  Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (triangle-fan (rounded-rectangle-vertices width height radius))
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] width)
  (preferred-height [this] height)

  Java2DDrawable
  (draw [this graphics]
    (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (let [[r g b a] (map float color)]
      (doto graphics
        (.setColor (Color. r g b a))
        (.fill (RoundRectangle2D$Float. 0 0 width height radius radius)))))

  Object
  (toString [this] (layoutable/describe-layoutable this "FilledRoundedRectangle" :radius)))

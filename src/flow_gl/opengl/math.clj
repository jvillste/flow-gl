(ns flow-gl.opengl.math)

(defn projection-matrix-2d [width height]
  [(/ 2.0 width)
   0.0
   0.0
   0.0

   0.0
   (/ -2.0 height)
   0.0
   0.0

   0.0
   0.0
   2.0
   0.0

   -1.0
   1.0
   1.0
   1.0])

(defn quad [x y width height]
  [x y
   x (+ y height)
   (+ x width) y

   x (+ y height)
   (+ x width) (+ y height)
   (+ x width) y])

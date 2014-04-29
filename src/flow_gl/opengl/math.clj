(ns flow-gl.opengl.math)

(defn projection-matrix-2d [width height depth]
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
   (/ 2.0 depth)
   0.0

   -1.0
   1.0
   1.0
   1.0])

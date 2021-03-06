(ns flow-gl.opengl.math
  (:require [clojure.core.matrix :as matrix]))

(defn core-matrix-to-opengl-matrix [core-matrix]
  (apply concat (matrix/columns core-matrix)))

(defn projection-matrix-2d [width height]
  (matrix/matrix [[(/ 2.0 width) 0.0             0.0 -1.0]
                  [0.0           (/ -2.0 height) 0.0 1.0]
                  [0.0           0.0             2.0 1.0]
                  [0.0           0.0             0.0 1.0]]))

(defn projection-matrix-3d [near far left right top bottom]
  (matrix/matrix [[(/ (* 2.0 near) (- right left)) 0.0 (/ (+ right left) (- right left)) 0.0]
                  
                  [0.0 (/ (* 2.0 near) (- top bottom)) (/ (+ top bottom) (- top bottom)) 0.0]

                  [0.0 0.0 (- (/ (+ far near) (- far near))) (- (/ (* 2.0 far near) (- far near)))]

                  [0.0  0.0  -1.0 0.0]]))

(defn projection-matrix-3d-orthogonal [near far left right top bottom]
  (matrix/matrix [[(/ 2.0 (- right left)) 0.0 0.0  (- (/ (+ right left) (- right left)))]
                  
                  [0.0 (/ 2.0 (- top bottom))  0.0 (- (/ (+ top bottom) (- top bottom)))]

                  [0.0 0.0 (- (/ 2.0 (- far near))) (- (/ (+ far near) (- far near)))]

                  [0.0  0.0  0.0 1.0]]))

(defn radians [degrees]
  (* Math/PI (/ degrees 180)))


(defn top [field-of-view near]
  (Math/tan (* near (/ field-of-view 2.0))))



(defn z-rotation-matrix [angle]
  (matrix/matrix [[(Math/cos angle) (- (Math/sin angle)) 0.0 0.0]
                  [(Math/sin angle) (Math/cos angle)     0.0 0.0]
                  [0.0              0.0                  1.0 0.0]
                  [0.0              0.0                  0.0 1.0]]))

(defn x-rotation-matrix [angle]
  (matrix/matrix [[1.0 0.0 0.0 0.0]
                  [0.0 (Math/cos angle) (- (Math/sin angle)) 0.0]
                  [0.0 (Math/sin angle) (Math/cos angle)     0.0]
                  [0.0              0.0     0.0             1.0]]))

(defn scaling-matrix [x y]
  (matrix/matrix [[x   0.0 0.0 0.0]
                  [0.0 y   0.0 0.0]
                  [0.0 0.0 1.0 0.0]
                  [0.0 0.0 0.0 1.0]]))

(defn translation-matrix [x y z]
  (matrix/matrix [[1.0 0.0 0.0 x]
                  [0.0 1.0 0.0 y]
                  [0.0 0.0 1.0 z]
                  [0.0 0.0 0.0 1.0]]))



(defn quad [x y width height]
  [x y
   x (+ y height)
   (+ x width) y

   x (+ y height)
   (+ x width) (+ y height)
   (+ x width) y])

(comment
  (matrix/mmul 
   (translation-matrix 0 0 0)
   [10 10 10 1])

  (matrix/mmul [[1 2]
                [1 1]]
               [1 2])

  (matrix/mmul #_(z-rotation-matrix (* 45 (/ Math/PI 180)))
               (translation-matrix 1 0)
               (matrix/identity-matrix 4)
               #_(matrix/matrix  [[1 1 1 1]
                                  [1 1 1 1]
                                  [1 1 1 1]
                                  [1 1 1 1]])
               #_(matrix/matrix [1 1 1 1])))


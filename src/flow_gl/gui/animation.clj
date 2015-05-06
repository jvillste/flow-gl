(ns flow-gl.gui.animation
  (:use clojure.test))

(defn sin-wave [from to duration-in-seconds time]
  (let [duration-in-nanoseconds (* duration-in-seconds 1e9 2)]
    (-> time
        (mod duration-in-nanoseconds)
        (/ duration-in-nanoseconds)
        (* 2 Math/PI)
        (+ (* 1.5 Math/PI))
        (Math/sin)
        (+ 1)
        (/ 2)
        (* (- to from))
        (+ from))))


(deftest sin-wave-test
  (are [time result] (= result
                        (sin-wave 1 2 1 time))
       0 1.0
       (- 1e9 1) 2.0
       (* 1e9 1/2) 1.5))



(ns flow-gl.gui.animation
  (:use clojure.test))

(defn sin-wave [from to duration-in-seconds time-in-millis]
  (let [duration-in-millis (* duration-in-seconds 1e6)]
    (-> time-in-millis
        (mod duration-in-millis)
        (/ duration-in-millis)
        (* 2 Math/PI)
        (+ (* 1.5 Math/PI))
        (Math/sin)
        (+ 1)
        (/ 2)
        (* (- to from))
        (+ from))))




(deftest sin-wave-test
  (are [time result] (= result
                        (sin-wave 0 10 1 time))
       0.0 0.0
       (* 1e6 2) 0.0
       (* 1e6 1) 0.0
       (* 1e6 1/2) 10.0))


(defn wave [time length from to]
    (+ (* (/ (Math/sin (* (/ (mod time length)
                             length)
                          Math/PI
                          2))
             2)
          (- to from))
       (/ (- to from)
          2)))

(run-tests)



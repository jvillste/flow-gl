(ns flow-gl.gui.animation
  (:require [flow-gl.dataflow :as dataflow])
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
                        (sin-wave 1 2 1 0 time))
       0 1.0
       (- 1e9 1) 2.0
       (* 1e9 1/2) 1.5))


(defn animation-started-path [animation]
  (keyword (str (name animation) "-animation-started")))

(defn animate-path [animation]
  (keyword (str "animate-" (name animation))))

(defn initialize-animation [animation-keyword]
  (let [animation-name (name animation-keyword)]
    (dataflow/initialize
     (animate-path animation-name) false
     (animation-started-path animation-name) 0)))

(defn animation-time [animation duration]
  (if (dataflow/get-value (animate-path animation))
    (let [time (dataflow/get-global-value :time)
          start-time (dataflow/get-value (animation-started-path animation))]
      (if (< (- time start-time)
             (* duration 1e9))
        (- time start-time)
        nil))
    nil))


(defn start-animation [state animation]
  (-> state
      (dataflow/define-to (animate-path animation) true)
      (dataflow/define-to (animation-started-path animation) (dataflow/get-global-value-from state :time))))

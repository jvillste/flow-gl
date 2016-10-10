(ns examples.hi-scene-graph
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layouts :as layouts]
                         [keyboard :as keyboard]
                         [visuals :as visuals])

            (flow-gl.graphics [font :as font])))

(defn draw-rectangle [width height color]
  (rectangle/create-buffered-image color width height 10 10))

(defn rectangle [x y width height color]
  {:x x
   :y y
   :width width
   :height height
   :image-function draw-rectangle
   :parameters [width height color]})

(defn text [x y z string]
  (let [font (font/create "LiberationSans-Regular.ttf" 40)]
    {:x x
     :y y
     :width (font/width font string)
     :height (font/height font)
     :image-function text/create-buffered-image
     :parameters [[255 255 255 255]
                  font
                  string]}))

(defn text-box [x y z value]
  {:x x :y y :z z
   :children [(rectangle 0 0 300 50 [0 0 255 150])
              (text 10 0 0 value)]})

(defn drop-down [x y z]
  {:x x :y y :z z
   :children (concat [(rectangle 0 0 150 250 [0 255 255 240])
                      (rectangle 0 0 150 50 [0 0 255 150])
                      (text 10 0 0 "drop")]
                     (for [[index value] (map-indexed vector ["down" "values"])]
                       (text 10 (+ 60 (* index 60)) 0 value)))})

(defn create-scene-graph [width height]
  {:x 100 :y 100
   :children (concat [(rectangle 0 0 320 370 [0 255 255 150])
                      (drop-down 160 10 1)]
                     (for [index (range 5)]
                       (text-box 10 (+ 70 (* index 60)) 0 (str "text box " index))))})

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  (application/start-window create-scene-graph)
  #_(.start (Thread. (fn []
                       (start-window)))))






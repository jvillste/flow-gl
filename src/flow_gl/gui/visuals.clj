(ns flow-gl.gui.visuals
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.font :as font]
            [flow-gl.graphics.rectangle :as rectangle]
            [flow-gl.graphics.text :as text]
            [fungl.handler :as handler]
            [fungl.util :as util]))

(defn hit-test-rectangle [{:keys [width height corner-arc-width corner-arc-height]} x y]
  (rectangle/contains width height corner-arc-width corner-arc-height x y))

(defn rectangle
  ([color corner-arc-width corner-arc-height]
   {:type ::rectangle
    :corner-arc-width corner-arc-width
    :corner-arc-height corner-arc-height
    :color color
    :image-function rectangle/create-buffered-image
    :draw-function rectangle/fill
    :image-function-parameter-keys [:color :width :height :corner-arc-width :corner-arc-height]
    :hit-test hit-test-rectangle})

  ([color corner-arc-width corner-arc-height width height]
   (assoc (rectangle color corner-arc-width corner-arc-height)
          :width width
          :height height)))

(defn- draw-rectangle-on-graphics [graphics width height draw-color fill-color line-width corner-arc-width corner-arc-height]
  (when fill-color
    (rectangle/fill graphics
                    fill-color
                    width
                    height
                    corner-arc-width
                    corner-arc-height))

  (when (> line-width 0)
    (rectangle/draw graphics
                    draw-color
                    line-width
                    width
                    height
                    corner-arc-width
                    corner-arc-height)))

(defn- draw-rectangle-2 [width height draw-color fill-color line-width corner-arc-width corner-arc-height]
  (let [buffered-image (buffered-image/create width height)]
    (draw-rectangle-on-graphics (buffered-image/get-graphics buffered-image)
                                width height draw-color fill-color line-width corner-arc-width corner-arc-height)
    buffered-image))

(def rectangle-node {:type ::rectangle
                     :image-function draw-rectangle-2
                     :draw-function draw-rectangle-on-graphics
                     :image-function-parameter-keys [:width :height :draw-color :fill-color :line-width :corner-arc-width :corner-arc-height]
                     :hit-test hit-test-rectangle})

(def default-rectangle-properties {:draw-color nil
                                   :line-width 0
                                   :fill-color [128 128 128 255]
                                   :corner-arc-size nil
                                   :corner-arc-width 0
                                   :corner-arc-height 0})

(defn rectangle-2 [& {:as properties
                      :keys [draw-color
                             line-width
                             fill-color
                             corner-arc-radius
                             corner-arc-width
                             corner-arc-height]}]

  (merge rectangle-node
         default-rectangle-properties
         properties
         (if corner-arc-radius
           {:corner-arc-width corner-arc-radius
            :corner-arc-height corner-arc-radius}
           {})))

(defn adapt-text-to-scale [node x-scale y-scale]
  (let [new-font-size (* (:font-size node)
                         (max x-scale y-scale))]
    (assoc node
           :font-size new-font-size
           :font (font/create (:font-file-path node)
                              new-font-size))))

(def liberation-sans-regular-path  (.getPath (io/resource "LiberationSans-Regular.ttf")))

(util/defno text [string] {color [255 255 255 255]
                           font-size 20
                           font-file-path liberation-sans-regular-path
                           font nil}

  (assert (string? string))
  (assert (vector? color))
  (assert (number? font-size))
  (assert (string? font-file-path))

  (let [font (or font
                 (font/create font-file-path
                              font-size))]
    {:type ::text
     :color color
     :font font
     :adapt-to-scale adapt-text-to-scale
     :string string
     :width (font/width font string)
     :height (font/height font)
     :image-function text/create-buffered-image
     :draw-function text/draw
     :image-function-parameter-keys [:color :font :string]}))



(handler/def-handler-creator create-text-area-adapt-to-space [color font string] [node]
  (assoc node :rows (if (and string (not= string ""))
                      (text/rows-for-text color
                                          font
                                          string
                                          (:available-width node))
                      nil)))


(handler/def-handler-creator create-text-area-get-size [font] [node]
  (if-let [rows (:rows node)]
    (text/rows-size rows)
    {:width 0
     :height (font/height font)}))

(def default-font (font/create (.getPath (io/resource "LiberationSans-Regular.ttf"))
                           30))

(defn text-area
  ([string color font]
   (assert (string? string)
           "Text area must be given a string")
   {:type ::text-area
    :color color
    :font font
    :string string
    :adapt-to-space (create-text-area-adapt-to-space color font string)

    :get-size (create-text-area-get-size font)

    :image-function text/create-buffered-image-for-rows
    :image-function-parameter-keys [:rows]})
  ([string color]
   (text-area string
              color
              default-font))
  ([string]
   (text-area string
              [255 255 255 255]
              default-font)))

(defn buffered-image-coordinate [buffered-image-max image-max image-coordinate]
  (int (* image-coordinate
          (/ buffered-image-max
             image-max))))

(deftest buffered-image-coordinate-test
  (is (= 100
         (buffered-image-coordinate 100 100 100)))

  (is (= 0
         (buffered-image-coordinate 100 100 0)))

  (is (= 100
         (buffered-image-coordinate 100 200 200))))


(defn hit-test-image [{:keys [buffered-image width height]} x y]
  (let [x (int (* x
                  (/ (- (.getWidth buffered-image)
                        1)
                     width)))
        y (int (* y
                  (/ (- (.getHeight buffered-image)
                        1)
                     width)))]
    (when (and (< x (.getWidth buffered-image))
               (< y (.getHeight buffered-image)))
      (= 255 (last (buffered-image/get-color buffered-image x y))))))



(defn image [buffered-image]
  {:buffered-image buffered-image
   :width (.getWidth buffered-image)
   :height (.getHeight buffered-image)
   :draw-function buffered-image/draw-image
   :image-function identity
   :image-function-parameter-keys [:buffered-image :width :height]
   :hit-test hit-test-image})

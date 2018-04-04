(ns flow-gl.gui.visuals
  (:require (fungl [cache :as cache]
                   [handler :as handler])
            [flow-gl.graphics.text :as text]
            
            [flow-gl.graphics.rectangle :as rectangle]

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            [clojure.java.io :as io])
  (:use clojure.test))

(defn draw-rectangle [width height color corner-arc-width corner-arc-height]
  (rectangle/create-buffered-image color width height corner-arc-width corner-arc-height))

(defn hit-test-rectangle [{:keys [width height corner-arc-width corner-arc-height]} x y]
  (rectangle/contains width height corner-arc-width corner-arc-height x y))

(defn rectangle
  ([color corner-arc-width corner-arc-height]
   {:type ::rectangle
    :corner-arc-width corner-arc-width
    :corner-arc-height corner-arc-height
    :color color
    :image-function draw-rectangle
    :image-function-parameter-keys [:width :height :color :corner-arc-width :corner-arc-height]
    :hit-test hit-test-rectangle})
  
  ([color corner-arc-width corner-arc-height width height]
   (assoc (rectangle color corner-arc-width corner-arc-height)
          :width width
          :height height)))

(defn- draw-rectangle-2 [width height draw-color fill-color line-width corner-arc-width corner-arc-height]
  (let [buffered-image (buffered-image/create width height)]
    (when fill-color
      (rectangle/fill (buffered-image/get-graphics buffered-image)
                      fill-color
                      width
                      height
                      corner-arc-width
                      corner-arc-height))

    (when (> line-width 0)
      (rectangle/draw (buffered-image/get-graphics buffered-image)
                      draw-color
                      line-width
                      width
                      height
                      corner-arc-width
                      corner-arc-height))

    
    
    buffered-image))

(def rectangle-defaults {:draw-color nil
                         :line-width 0
                         :fill-color [128 128 128 255]
                         :corner-arc-width 0
                         :corner-arc-height 0

                         :type ::rectangle
                         :image-function draw-rectangle-2
                         :image-function-parameter-keys [:width :height :draw-color :fill-color :line-width :corner-arc-width :corner-arc-height]
                         :hit-test hit-test-rectangle})

(defn rectangle-2 [& properties]
  (merge rectangle-defaults
         (apply hash-map properties)))

(defn adapt-text-to-scale [node x-scale y-scale]
  (let [new-font-size (* (:font-size node)
                         (max x-scale y-scale))]
    (assoc node
           :font-size new-font-size
           :font (font/create (:font-file-path node)
                              new-font-size))))

(defn text
  ([string color font-size font-file-path]
   (assert (string? string))
   (assert (vector? color))
   (assert (number? font-size))
   (assert (string? font-file-path))
   
   (let [font (font/create font-file-path
                           font-size)]
     {:type ::text
      :color color
      :font font
      :font-size font-size
      :font-file-path font-file-path
      :adapt-to-scale adapt-text-to-scale
      :string string
      :width (font/width font string)
      :height (font/height font)
      :image-function text/create-buffered-image
      :image-function-parameter-keys [:color :font :string]}))

  ([string color font-size]
   (text string
         color
         font-size
         (.getPath (io/resource "LiberationSans-Regular.ttf"))))
  
  ([string color]
   (text string
         color
         18
         (.getPath (io/resource "LiberationSans-Regular.ttf"))))
  
  ([string]
   (text string
         [255 255 255 255]
         50
         (.getPath (io/resource "LiberationSans-Regular.ttf")))))



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
   :image-function identity
   :image-function-parameter-keys [:buffered-image]
   :hit-test hit-test-image})



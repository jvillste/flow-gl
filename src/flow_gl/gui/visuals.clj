(ns flow-gl.gui.visuals
  (:require (fungl [cache :as cache]
                   [handler :as handler])
            [flow-gl.graphics.text :as text]
            
            [flow-gl.graphics.rectangle :as rectangle]

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:use clojure.test))

(defn draw-rectangle [width height color corner-arc-width corner-arc-height]
  (rectangle/create-buffered-image color width height corner-arc-width corner-arc-height))

(defn hit-test-rectangle [{:keys [width height corner-arc-width corner-arc-height]} x y]
  (rectangle/contains width height corner-arc-width corner-arc-height x y))

(defn rectangle [color corner-arc-width corner-arc-height]
  {:type ::rectangle
   :corner-arc-width corner-arc-width
   :corner-arc-height corner-arc-height
   :color color
   :image-function draw-rectangle
   :image-function-parameter-keys [:width :height :color :corner-arc-width :corner-arc-height]
   :hit-test hit-test-rectangle})

(defn text
  ([color font string]
   {:type ::text
    :color color
    :font font
    :string string
    :width (font/width font string)
    :height (font/height font)
    :image-function text/create-buffered-image
    :image-function-parameter-keys [:color :font :string]})

  ([color string]
   (text color
         (font/create "LiberationSans-Regular.ttf" 18)
         string))
  
  ([string]
   (text [255 255 255 255]
         (font/create "LiberationSans-Regular.ttf" 18)
         string)))


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

(defn text-area [color font string]
  {:type ::text-area
   :color color
   :font font
   :string string
   :adapt-to-space (create-text-area-adapt-to-space color font string)
   
   :get-size (create-text-area-get-size font)
   
   :image-function text/create-buffered-image-for-rows
   :image-function-parameter-keys [:rows]})

(defn hit-test-image [{:keys [buffered-image width height]} x y]
  (let [x (int (* x (/ (- (.getWidth buffered-image)
                          1)
                       width)))
        y (int (* y (/ (- (.getHeight buffered-image)
                          1)
                       width)))]
    (= 255 (last (buffered-image/get-color buffered-image x y)))))

(defn image [buffered-image]
  {:buffered-image buffered-image
   :width (.getWidth buffered-image)
   :height (.getHeight buffered-image)
   :image-function (fn [] buffered-image)
   :image-function-parameter-keys []
   :hit-test hit-test-image})


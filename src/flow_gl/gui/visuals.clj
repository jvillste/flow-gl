(ns flow-gl.gui.visuals
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.font :as font]
            [flow-gl.graphics.rectangle :as rectangle]
            [flow-gl.graphics.text :as text]
            [fungl.handler :as handler]
            [fungl.util :as util]
            [fungl.swing.root-renderer :as root-renderer]
            [fungl.cache :as cache]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.renderer :as renderer]))



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



(defn text-area-adapt-to-space [color font string node]
  (-> node
      (assoc :rows (if (and string (not= string ""))
                     (text/rows-for-text color
                                         font
                                         string
                                         (:available-width node))
                     nil))
      (dissoc :adapt-to-space)))

(defn text-area-get-size [font node]
  (if-let [rows (:rows node)]
    (text/rows-size rows)
    {:width 0
     :height (font/height font)}))

(defn default-font []
  (font/create-by-name "CourierNewPSMT" 50)
  #_(font/create (.getPath (io/resource "LiberationSans-Regular.ttf"))
                 30))

(defn liberation-sans-regular [size]
  (font/create liberation-sans-regular-path
               size))

(defn text-area
  ([string color font]
   (assert (string? string)
           "Text area must be given a string")
   {:type ::text-area
    :color color
    :font font
    :string string
    :adapt-to-space [text-area-adapt-to-space color font string]

    :get-size [text-area-get-size font]

    :image-function text/create-buffered-image-for-rows
    :draw-function text/draw-rows
    :image-function-parameter-keys [:rows]})
  ([string color]
   (text-area string
              color
              (default-font)))
  ([string]
   (text-area string
              [255 255 255 255]
              (default-font))))

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

(def ^:dynamic image-cache)

(defn state-bindings []
  {#'image-cache (cache/create-state 30)})

(defn layers [nodes]
  (->> nodes
       (group-by :z)
       (map (fn [[z layer-nodes]]
              {:z z
               :nodes layer-nodes
               :bounding-box (scene-graph/bounding-box nodes)}))))

(defn bounding-box-contains? [inner-bounding-box outer-bounding-box]
  (and (<= (:x outer-bounding-box)
           (:x inner-bounding-box))

       (<= (:y outer-bounding-box)
           (:y inner-bounding-box))

       (<= (+ (:x inner-bounding-box)
              (:width inner-bounding-box))
           (+ (:x outer-bounding-box)
              (:width outer-bounding-box)))

       (<= (+ (:y inner-bounding-box)
              (:height inner-bounding-box))
           (+ (:y outer-bounding-box)
              (:height outer-bounding-box)))))

(defn merge-contained-layer [inner-layer outer-layer]
  {:bounding-box (:bounding-box outer-layer)
   :nodes (concat (:nodes outer-layer)
                  (:nodes inner-layer))
   :z (:z inner-layer)})

(defn merge-contained-layers [layers]
  (let [layers (sort-by :z layers)]
    (loop [previous-layer (first layers)
           layers (rest layers)
           merged-layers []]
      (let [layer (first layers)]
        (if (nil? layer)
          (conj merged-layers previous-layer)
          (if (bounding-box-contains? (:bounding-box layer)
                                      (:bounding-box previous-layer))
            (recur (merge-contained-layer layer
                                          previous-layer)
                   (rest layers)
                   merged-layers)
            (recur layer
                   (rest layers)
                   (conj merged-layers
                         previous-layer))))))))

(deftest test-merge-contained-layers
  (is (= [{:bounding-box {:x 0, :y 0, :width 2, :height 2}, :nodes '(1 2), :z 1}]
         (merge-contained-layers [{:z 0 :nodes [1] :bounding-box {:x 0 :y 0 :width 2 :height 2}}
                                  {:z 1 :nodes [2] :bounding-box {:x 0 :y 0 :width 1 :height 1}}])))

  (is (= [{:z 0, :nodes [1], :bounding-box {:x 0, :y 0, :width 2, :height 2}}
          {:z 1, :nodes [2], :bounding-box {:x 0, :y 0, :width 10, :height 1}}]
         (merge-contained-layers [{:z 0 :nodes [1] :bounding-box {:x 0 :y 0 :width 2 :height 2}}
                                  {:z 1 :nodes [2] :bounding-box {:x 0 :y 0 :width 10 :height 1}}])))

  (is (= [{:z 0, :nodes [1], :bounding-box {:x 0, :y 0, :width 2, :height 2}}
          {:bounding-box {:x 0, :y 0, :width 10, :height 1}, :nodes '(2 3), :z 2}]
         (merge-contained-layers [{:z 0 :nodes [1] :bounding-box {:x 0 :y 0 :width 2 :height 2}}
                                  {:z 1 :nodes [2] :bounding-box {:x 0 :y 0 :width 10 :height 1}}
                                  {:z 2 :nodes [3] :bounding-box {:x 0 :y 0 :width 5 :height 1}}]))))

(defn layer-to-image [original-node layer]
  (assoc (image (root-renderer/render-to-buffered-image (:bounding-box layer)
                                                        (:sort-by :z (:nodes layer))))
         :id (:id original-node)
         :z (:z layer)
         :x (- (:x (:bounding-box layer))
               (:x original-node))
         :y (- (:y (:bounding-box layer))
               (:y original-node))))

(defn render-to-images [original-node]
  #_(prn 'render-to-images (:id original-node)) ;; TODO: remove me

  (assoc (select-keys original-node [:x :y :z :width :height :id])
         :children (->> (renderer/apply-renderers! original-node
                                                   nil)
                        (scene-graph/leaf-nodes)
                        (filter :draw-function)
                        (layers)
                        (merge-contained-layers)
                        (map (partial layer-to-image
                                      original-node))
                        (doall))

         #_(doall (map (fn [[z leaf-nodes]]

                         (let [bounding-box (scene-graph/bounding-box leaf-nodes)]
                           (assoc (image (root-renderer/render-to-buffered-image bounding-box
                                                                                 leaf-nodes))
                                  :id (:id original-node)
                                  :z z
                                  :x (- (:x bounding-box)
                                        (:x original-node))
                                  :y (- (:y bounding-box)
                                        (:y original-node)))))
                       (group-by :z (filter :draw-function (scene-graph/leaf-nodes (renderer/apply-renderers! original-node
                                                                                                              nil))))))))

(defn render-to-images-render-function [_graphics scene-graph]
  #_(prn 'render-to-images-render-function (:id scene-graph)) ;; TODO: remove me

  #_(render-to-images (dissoc scene-graph
                              :render
                              :render-on-descend?))
  (cache/call-with-cache image-cache
                         render-to-images
                         (dissoc scene-graph
                                 :render
                                 :render-on-descend?))
  #_(render-to-images scene-graph))

(defn render-cache [child]
  {:children [child]
   :render render-to-images-render-function})

(ns examples.kurpitsa
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layouts :as layouts]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [animation :as animation])

            (flow-gl.graphics [font :as font]
                              [rectangle :as rectangle]
                              [text :as text]
                              [buffered-image :as buffered-image])))

(defn draw-rectangle [width height color]
  (rectangle/create-buffered-image color width height 20 20))

(defn rectangle [x y width height color]
  {:x x
   :y y
   :width width
   :height height
   :color color
   :image-function draw-rectangle
   :image-function-parameter-keys [:width :height :color]})

(def font (font/create "LiberationSans-Regular.ttf" 40))

(defn text [x y z string]
  {:x x
   :y y
   :width (font/width font string)
   :height (font/height font)
   :image-function text/create-buffered-image
   :color [255 255 255 255]
   :font font
   :string string
   :image-function-parameter-keys [:color
                                   :font
                                   :string]})

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

(def pumpkin (buffered-image/create-from-file "pumpkin.png"))

(defn image [buffered-image]
  {:buffered-image buffered-image
   #_:get-size #_(fn [node]
                   {:width (.getWidth buffered-image)
                    :height (.getHeight buffered-image)})
   :image-function (fn [buffered-image] buffered-image)
   :image-function-parameter-keys [:buffered-image]})

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 500)
  (println (* 100 (float (animation/phase! :width
                                           (partial animation/linear-phaser 5000)
                                           (fn [p] p) #_(partial animation/limit! 0 1)))))
  {:x 0 :y 0
   :children (let [width 1000
                   height 100]
               [(assoc (image pumpkin)
                       :x 0 #_(* x width)
                       :y 0 #_(* y height)
                       :width 100 #_(* 100 (animation/phase! :width
                                                             
                                                             (partial animation/linear-phaser 1000)
                                                             
                                                             (animation/time!)
                                                             (fn [phase] phase)))
                       :height height)])
   #_(let [width 100 #_(+ 10 (* 100 (animation/phase! :width
                                                      
                                                      (partial animation/linear-phaser 1000)
                                                      
                                                      (animation/time!)
                                                      (fn [phase] phase))))
           height 100]
       #_(flatten (for [x 10 #_(range (if (> (mod (animation/time!)
                                                  1000)
                                             500)
                                        1
                                        10))]
                    (for [y (range 2)]
                      (assoc (image pumpkin)
                             :x (* x width)
                             :y (* y height)
                             :width width
                             :height height)))))
   

   #_[ ] #_(concat [(image pumpkin)
                    #_(rectangle 0 0 320 370 [0 255 255 150])
                    #_(drop-down 160 10 1)]
                   #_(for [index (range 5)]
                       (text-box 10 (+ 70 (* index 60)) 0 (str "text box " index))))})

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  #_(application/start-window #'create-scene-graph)
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))



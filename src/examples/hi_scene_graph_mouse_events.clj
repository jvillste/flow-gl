(ns examples.hi-scene-graph-mouse-events
  (:require [clojure.spec.test :as spec-test]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            [clojure.core.async :as async]
            (flow-gl.gui [window :as window]
                         [quad-renderer :as quad-renderer]
                         [scene-graph :as scene-graph]
                         [mouse :as mouse])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target]))
  (:use flow-gl.utils
        clojure.test))

(spec-test/instrument)

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
   :children [(assoc (rectangle 0 0 300 50 [0 0 255 150])
                     :mouse-event-handler (fn [event]
                                            (println "got event in text box" value event)
                                            event))
              (text 10 0 0 value)]})

(defn drop-down [x y z]
  {:x x :y y :z z
   :children (concat [(assoc (rectangle 0 0 150 250 [0 255 255 240])
                             :mouse-event-handler (fn [event]
                                                    (println "got event in drop down" event)
                                                    event))
                      (rectangle 0 0 150 50 [0 0 255 150])
                      (text 10 0 0 "drop")]
                     (for [[index value] (map-indexed vector ["down" "values"])]
                       (text 10 (+ 60 (* index 60)) 0 value)))})

(defn nodes []
  {:x 100 :y 100
   :children (concat [(rectangle 0 0 320 370 [0 255 255 150])
                      (drop-down 160 10 1)]
                     (for [index (range 5)]
                       (text-box 10 (+ 70 (* index 60)) 0 (str "text box " index))))})

(defn start-window []
  (let [window-width 800
        window-height 800
        window (jogl-window/create window-width
                                   window-height
                                   :close-automatically true)
        event-channel (window/event-channel window)]

    (loop [quad-renderer (window/with-gl window gl (quad-renderer/create gl))]
      
      (when (window/visible? window)
        (recur (try
                 
                 (let [scene-graph (nodes)
                       quad-renderer (window/with-gl window gl
                                       (opengl/clear gl 0 0 0 1)

                                       (quad-renderer/draw quad-renderer
                                                           (scene-graph/leave-nodes scene-graph)
                                                           window-width
                                                           window-height
                                                           gl))]
                   (window/swap-buffers window)

                   (let [event (async/<!! event-channel)]
                     (when (= :mouse (:source event))
                       (mouse/call-mouse-event-handlers scene-graph event)))
                   
                   quad-renderer)
                 
                 (catch Throwable e
                   (.printStackTrace e *out*)
                   (window/close window)
                   (throw e))))))
    (println "exiting")))


(defn start []
  #_(start-window)
  (.start (Thread. (fn []
                       (start-window)))))





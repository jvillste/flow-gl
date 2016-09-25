(ns examples.hi-quad-renderer
  (:require [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            (flow-gl.gui [window :as window]
                         [quad-renderer :as quad-renderer])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target]))
  (:use flow-gl.utils
        clojure.test))

(defn draw-rectangle [width height color corner-arch-width corner-arc-height]
  (rectangle/create-buffered-image color width height corner-arch-width corner-arc-height))

(defn start []
  (let [window (jogl-window/create 500
                                   500
                                   :close-automatically true)]

    (window/with-gl window gl
      (let [quad-renderer (quad-renderer/create  gl)
            icon (buffered-image/create-from-file "pumpkin.png")
            font (font/create "LiberationSans-Regular.ttf" 40)
            window-size (opengl/size gl)]

        
        
        (opengl/clear gl 0 0 0 1)

        (quad-renderer/draw quad-renderer
                            (concat [{:x 150
                                        :y 10
                                        :width (.getWidth icon)
                                        :height (.getHeight icon)
                                        :image-function (fn [] icon)}
                                     
                                     {:x 100
                                      :y 200
                                      :width 200
                                      :height 20
                                      :image-function text/create-buffered-image
                                      :parameters [[255 255 255 255]
                                                   font
                                                   "Hello quad renderer!"]}

                                     {:x 100
                                      :y 240
                                      :width 200
                                      :height 200
                                      :image-function text/create-buffered-image
                                      :parameters [[255 255 255 255]
                                                   font
                                                   "Hello quad renderer!"]}])
                            
                            (:width window-size)
                            (:height window-size)
                            gl)))

    (window/swap-buffers window)))






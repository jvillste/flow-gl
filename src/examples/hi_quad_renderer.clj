(ns examples.hello-quad-renderer
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
  (let [window (jogl-window/create 800
                                   800
                                   :close-automatically true)]

    (window/with-gl window gl
      (let [quad-renderer (quad-renderer/create  gl)
            icon (buffered-image/create-from-file "pumpkin.png")
            font (font/create "LiberationSans-Regular.ttf" 40)
            text ( )
            window-size (opengl/size gl)]

        
        
        (opengl/clear gl 0 0 0 1)

        (quad-renderer/draw quad-renderer
                            (concat [#_{:x 150
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
                                                   "Hello quad batch!"]}

                                     {:x 100
                                      :y 10
                                      :width 100
                                      :height 20
                                      :image-function text/create-buffered-image
                                      :parameters [[255 255 255 255]
                                                   font
                                                   "Hello quad batch!"]}]
                                    
                                    #_(for [i (range 50)]
                                      {:x (* (/ (* i i) (* 50 50))
                                             700)
                                       :y 300
                                       :width i
                                       :height 100
                                       :width-dependent true
                                       :height-dependent true
                                       :image-function draw-rectangle
                                       :parameters [[255 255 255 155] 5 5]}))
                            
                            (:width window-size)
                            (:height window-size)
                            gl)))

    (window/swap-buffers window)))






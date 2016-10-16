(ns examples.hi-quad-renderer
  (:require [flow-gl.graphics.text :as text]
            (flow-gl.gui [window :as window]
                         [quad-renderer :as quad-renderer])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]))
  (:use flow-gl.utils
        clojure.test))

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
                                      :color [255 255 255 255]
                                      :font font
                                      :text "Hi quad renderer!"
                                      :image-function text/create-buffered-image
                                      :image-function-parameter-keys [:color :font :text]}

                                     {:x 100
                                      :y 240
                                      :width 200
                                      :height 200
                                      :color [255 255 255 255]
                                      :font font
                                      :text "Hi quad renderer!"
                                      :image-function text/create-buffered-image
                                      :image-function-parameter-keys [:color :font :text]}])
                            
                            (:width window-size)
                            (:height window-size)
                            gl)))

    (window/swap-buffers window)))






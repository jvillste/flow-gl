(ns examples.hi-quad-batch
  (:require [flow-gl.graphics.text :as text]
            (flow-gl.gui [window :as window])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad-batch :as quad-batch]
                                 [render-target :as render-target]))
  (:use flow-gl.utils
        clojure.test))


(defn start []
  (let [window (jogl-window/create 800
                                   800
                                   :close-automatically true)]

    (window/with-gl window gl
      (let [quad-batch (quad-batch/create gl)
            icon (buffered-image/create-from-file "pumpkin.png")
            text (text/create-buffered-image [255 255 255 255]
                                             (font/create "LiberationSans-Regular.ttf" 40)
                                             "Hello quad batch!")
            quad-batch (quad-batch/add-textures quad-batch
                                                gl
                                                [icon
                                                 text])
            window-size (opengl/size gl)]
        
        (opengl/clear gl 0 0 0 1)

        (quad-batch/draw-quads quad-batch
                               gl
                               [{:x 150
                                 :y 10
                                 :width (.getWidth icon)
                                 :height (.getHeight icon)
                                 :texture-id 0}
                                {:x 100
                                 :y 200
                                 :width (.getWidth text)
                                 :height (.getHeight text)
                                 :texture-id 1}]
                               (:width window-size)
                               (:height window-size))))

    (window/swap-buffers window)))







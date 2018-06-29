(ns examples.gl-texture-batch
  (:require [flow-gl.graphics.text :as text]
            (flow-gl.gui [window :as window]
                         [quad-renderer :as quad-renderer])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [texture :as texture])
            [clojure.java.io :as io])
  (:use flow-gl.utils
        clojure.test))

(defn start []
  (let [window (jogl-window/create 500
                                   500
                                   :close-automatically true)]

    (window/with-gl window gl
      (let [quad-renderer (quad-renderer/initialize-state gl)
            gl-texture (texture/create-for-file (.getPath (io/resource "pumpkin.png"))
                                                gl)
            window-size (opengl/size gl)]

        (opengl/clear gl 0 0 0 1)

        (quad-renderer/draw quad-renderer
                            (concat [{:x 250
                                      :y 10
                                      :width (.getWidth icon)
                                      :height (.getHeight icon)
                                      :texture-id gl-texture
                                      :texture-hash 1}])
                            
                            (:width window-size)
                            (:height window-size)
                            gl)))

    (window/swap-buffers window)))

(ns examples.render-target
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [render-target :as render-target])
            (flow-gl.gui [window :as window]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defn draw-rectangle [nanovg x y width height r g b a]
  (doto nanovg
    (NanoVG/fillColor (char r) (char g) (char b) (char a))
    (NanoVG/beginPath)
    (NanoVG/moveTo 10 10)
    (NanoVG/lineTo 100 100)
    (NanoVG/lineTo 10 100)
    (NanoVG/closePath)
    ;;(NanoVG/rect x y width height)
    (NanoVG/fill)))

(defn start-view []
  (let [window (jogl-window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)
        render-target-width 200
        render-target-height 200
        
        render-target (window/with-gl window gl
                        (render-target/create render-target-width render-target-height
                                              gl))

        nanovg  (window/with-gl window gl
                  (NanoVG/init))]

    (try
      (window/with-gl window gl
                          (let [{:keys [width height]} (opengl/size gl)]
                            (opengl/clear gl 0 0 0 1)

                            (render-target/render-to render-target gl
                                                     (opengl/clear gl 1 0 0 1)

                                                     (NanoVG/beginFrame nanovg render-target-width render-target-height)
                                                     (draw-rectangle nanovg
                                                                     0 0 100 100
                                                                     0 255 0 255)
                                                     (NanoVG/endFrame nanovg))

                            (render-target/draw render-target 0 0 width height gl)))
      (window/swap-buffers window)

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))


(defn start []
  (start-view))

(run-tests)

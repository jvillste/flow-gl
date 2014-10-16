(ns examples.render-target
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [render-target :as render-target]))
  (:use clojure.test)
  (:import [nanovg NanoVG]))

(defn draw-rectangle [nanovg x y width height r g b a]
  (doto nanovg
    (NanoVG/fillColor (char r) (char g) (char b) (char a))
    (NanoVG/beginPath)
    (NanoVG/rect x y width height)
    (NanoVG/fill)))

(defn start-view []
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :close-automatically true)
        
        render-target (window/with-gl window gl
                        (render-target/create 200 300
                                              gl))

        nanovg  (window/with-gl window gl
                  (NanoVG/init))]

    (try
      (window/set-display window gl
                          (let [{:keys [width height]} (opengl/size gl)]
                            (opengl/clear gl 0 0 0 1)

                            #_(render-target/start-rendering render-target gl)

                            (opengl/clear gl 1 1 0 1)

                            (NanoVG/beginFrame nanovg width height)
                            (draw-rectangle nanovg
                                            0 0 100 100
                                            0 255 0 255)
                            (NanoVG/endFrame nanovg)

                            

                            #_(render-target/end-rendering render-target gl)

                            #_(render-target/draw render-target width height gl)))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))


(defn start []
  (start-view))

(run-tests)

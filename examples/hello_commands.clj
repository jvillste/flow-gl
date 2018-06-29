(ns examples.hello-commands
  (:require (flow-gl.graphics [command :as command]
                              [font :as font])
            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window])))

(defn start []
  (let [width 300
        height 300
        window (window/create width
                              height
                              opengl/initialize
                              opengl/resize)

        text-command (text/create "Hello World "
                                  (font/create "LiberationSans-Regular.ttf" 20)
                                  [1 1 1 1])
        translate-command (translate/create 50 50)]


    (window/render window gl
                   (let [text-runner (command/create-runner text-command gl)
                         translate-runner (command/create-runner translate-command gl)]
                     
                     (command/run text-runner gl)
                     (command/run translate-runner gl)
                     (command/run text-runner gl)
                     
                     (command/delete text-runner gl)
                     (command/delete translate-runner gl)))))

(comment (start))

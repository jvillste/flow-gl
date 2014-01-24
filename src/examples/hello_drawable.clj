(ns examples.hello-drawable
  (:require (flow-gl.gui [drawable :as drawable])
            (flow-gl.graphics [command :as command]
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

        text-drawable (drawable/->Text "Hello World "
                                       (font/create "LiberationSans-Regular.ttf" 20)
                                       [1 1 1 1])

        rectangle-drawable (drawable/->Rectangle 105 25 [0 0 0.7 1])

        commands (concat (drawable/drawing-commands rectangle-drawable)
                         (drawable/drawing-commands text-drawable))]


    (window/render window gl
                   (let [runners (for [command commands]
                                   (command/create-runner command gl))]

                     (doseq [runner runners]
                       (command/run runner gl))

                     (doseq [runner runners]
                       (command/delete runner gl))))))

(comment (start))

(ns examples.hello-layouts
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout])
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
                                       (font/create "LiberationSans-Regular.ttf" 40)
                                       [1 1 1 1])

        layoutable (layout/->Margin 20 20 20 20
                                    [(layout/->VerticalStack [text-drawable
                                                              text-drawable])])

        layout (layout/layout layoutable
                              width
                              height)

        commands (drawable/drawing-commands layout)]


    (window/render window gl
                   (doseq [command commands]
                     (doto (command/create-runner command gl)
                       (command/run gl)
                       (command/delete gl))))))

;;(start)


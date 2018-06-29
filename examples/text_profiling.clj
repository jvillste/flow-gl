(ns examples.text-profiling
  (:require (flow-gl.gui [drawable :as drawable])
            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [frame-buffer :as frame-buffer]
                                 [texture :as texture])))

(defmacro named-time [name expression]
  `(do (println ~name)
       (time ~expression)))

(defn start []
  (let [width 800
        height 200
        window (window/create width
                              height
                              opengl/initialize
                              opengl/resize)

        drawables (named-time "creating drawables"
                              (doall (for [i (range 20)]
                                       (drawable/->Text (str "Hello asfdsadfsdf asf sadfsad af adf asdfas df sadf sf fsdf dfsd fsdaff asfsa" i)
                                                        (font/create "LiberationSans-Regular.ttf" 12)
                                                        [1 1 1 1]))))

        commands (named-time "creating commands"
                             (apply concat (doall (map drawable/drawing-commands drawables))))]


    (window/render window gl
                   (let [frame-buffer (frame-buffer/create)
                         texture (texture/create 100 50 gl)
                         runners (named-time "creating runners"
                                             (doall (map #(command/create-runner % gl)
                                                         commands)))]
                     (frame-buffer/bind gl frame-buffer)
                     (frame-buffer/bind-texture gl )

                     (named-time "running"
                                 (doseq [runner runners]
                                   (command/run runner gl)))


                     (named-time "running"
                                 (doseq [runner runners]
                                   (command/run runner gl)))

                     (named-time "running"
                                 (doseq [runner runners]
                                   (command/run runner gl)))

                     (named-time "deleting"
                                 (doseq [runner runners]
                                   (command/delete runner gl)))))))

(comment (start))

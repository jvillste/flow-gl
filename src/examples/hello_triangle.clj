(ns examples.hello-triangle
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list])))
(defn start []
  (let [width 300
        height 300
        window (window/create width height)
        gl (window/start-rendering window)]

    (opengl/initialize gl)
    (opengl/resize gl width height)


    (-> (triangle-list/create-for-coordinates gl
                                              :triangles
                                              [0 0
                                               width 0
                                               (/ width 2) height]
                                              [1 0 0 1
                                               0 1 0 1
                                               0 0 1 1])
        (triangle-list/render gl)
        (triangle-list/delete gl))
    (opengl/dispose gl)
    (window/end-rendering window)))

(comment (start))

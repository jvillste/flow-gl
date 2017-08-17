(ns examples.hello-triangle
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]))
  (:import [com.jogamp.opengl GL2]))

(defn start []
  (let [width 300
        height 300
        margin 100
        triangle-width (- width (* 2 margin))
        triangle-height (- height (* 2 margin))
        window (window/create width
                              height
                              :profile :gl3
                              :close-automatically true)]

    (window/render window gl
                   (opengl/initialize gl)
                   (triangle-list/create-shared-resources gl)

                   (-> (triangle-list/create-for-coordinates gl
                                                             :triangles
                                                             [0 0
                                                              triangle-width 0
                                                              (/ triangle-width 2) triangle-height]
                                                             [1 0 0 1
                                                              0 1 0 1
                                                              0 0 1 1])
                       (triangle-list/render gl width height)
                       (triangle-list/delete gl))

                   (triangle-list/delete-shared-resources gl))))

(comment (start))

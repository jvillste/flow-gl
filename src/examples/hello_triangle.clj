(ns examples.hello-triangle
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]))
  (:import [javax.media.opengl GL2]))

(defn start []
  (let [width 300
        height 300
        margin 100
        triangle-width (- width (* 2 margin))
        triangle-height (- height (* 2 margin))
        window (window/create width
                              height
                              opengl/initialize
                              opengl/resize)]

    (window/render window gl
                   (.glMatrixMode gl GL2/GL_MODELVIEW)
                   (.glTranslatef gl margin margin 0)

                   (-> (triangle-list/create-for-coordinates gl
                                                             :triangles
                                                             [0 0
                                                              triangle-width 0
                                                              (/ triangle-width 2) triangle-height]
                                                             [1 0 0 1
                                                              0 1 0 1
                                                              0 0 1 1])
                       (triangle-list/render gl)
                       (triangle-list/delete gl)))))

(comment (start))

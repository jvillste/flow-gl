(ns flow-gl.gui.transformers
  (:require
   (flow-gl.gui [renderer :as renderer]
                [layout :as layout]
                [drawable :as drawable]
                [gui :as gui])

   (flow-gl.opengl.jogl [opengl :as opengl]
                        [render-target :as render-target]
                        [quad :as quad]
                        [shader :as shader]))
  (:import [nanovg NanoVG]
           [flow_gl.gui.drawable Quad]
           [javax.media.opengl GL2])
  (:use clojure.test))



(defn ensure-render-target [render-target width height gl]
  (if render-target
    (if (and (= width
                (:width render-target))
             (= height
                (:height render-target)))
      render-target
      (do (render-target/delete render-target gl)
          (render-target/create width height gl)))
    (render-target/create width height gl)))


  (def textured-fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;

  out vec4 outColor;

  void main() {
  outColor = texture(texture, texture_coordinate);
  }
")

(def clip
  {:transformer (fn [layout gpu-state state]
                  (let [gl (:gl gpu-state)
                        width (:width layout)
                        height (:height layout)
                        render-target-1 (ensure-render-target (:render-target state) width height gl)
                        gpu-state (render-target/render-to render-target-1 gl
                                                           (opengl/clear gl 0 0 0 0)
                                                           (-> (assoc gpu-state :drawables (gui/drawables-for-layout (assoc layout
                                                                                                                            :x 0
                                                                                                                            :y 0)))
                                                               (gui/render-drawables)))]
                    [(assoc (drawable/->Quad ["texture" (:texture render-target-1)]
                                             []
                                             textured-fragment-shader-source
                                             (:x layout) (:y layout) width height)
                            :content-hash (hash layout))
                     gpu-state
                     (assoc state
                            :render-target render-target-1)]))
   
   :destructor (fn [state gl]
                 (when-let [render-target (:render-target state)]
                   (render-target/delete render-target gl)))})



(def cache
  {:transformer (fn [layout gpu-state state]
                  (let [gl (:gl gpu-state)
                        width (:width layout)
                        height (:height layout)
                        render-target-1 (ensure-render-target (:render-target state) width height gl)
                        gpu-state (if (= layout (:previous-layout state))
                                    gpu-state
                                    (render-target/render-to render-target-1 gl
                                                             (opengl/clear gl 0 0 0 0)
                                                             (-> (assoc gpu-state :drawables (gui/drawables-for-layout (assoc layout
                                                                                                                              :x 0
                                                                                                                              :y 0)))
                                                                 (gui/render-drawables))))]
                    
                    [(drawable/->Quad ["texture" (:texture render-target-1)]
                                      []
                                      textured-fragment-shader-source
                                      (:x layout) (:y layout) width height)
                     gpu-state
                     (assoc state
                            :previous-layout layout
                            :render-target render-target-1)]))
   
   :destructor (fn [state gl]
                 (when-let [render-target (:render-target state)]
                   (render-target/delete render-target gl)))})

(defn blur-transformer [radius layout gpu-state state]
  #_(flow-gl.tools.trace/log "blur trans" layout state)
  (let [gl (:gl gpu-state)
        width (:width layout)
        height (:height layout)
        render-target (if-let [render-target (:render-target state)]
                        (if (and (= width
                                    (:width render-target))
                                 (= height
                                    (:height render-target)))
                          render-target
                          (do (render-target/delete render-target gl)
                              (render-target/create width height gl)))
                        (render-target/create width height gl))
        state (assoc state :render-target render-target)
        gpu-state (render-target/render-to render-target gl
                                           #_(flow-gl.tools.trace/log "render to render target" layout)
                                           (opengl/clear gl 0 0 0 1)
                                           (-> (assoc gpu-state :drawables (gui/drawables-for-layout (assoc layout
                                                                                                            :x 0
                                                                                                            :y 0)))
                                               (gui/render-drawables)))]
    
    [(assoc (drawable/->Quad ["texture" (:texture render-target)]
                             [:1f "resolution" width
                              :1f "radius" radius
                              :2f "dir" [1.0 0.0]]
                             quad/blur-fragment-shader-source
                             (:x layout) (:y layout) width height)
            :layout-hash (hash layout))

     gpu-state
     state]))

(def blur
  {:transformer #'blur-transformer})


#_(run-all-tests)

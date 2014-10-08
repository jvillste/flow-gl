(ns flow-gl.gui.renderer
  (:require 
            (flow-gl.gui [drawable :as drawable]
                         [quad-view :as quad-view])

            (flow-gl.opengl.jogl [opengl :as opengl]))
  (:import [nanovg NanoVG]
           [javax.media.opengl GL2]))

(defprotocol Renderer
  (can-draw? [this drawable])

  (draw-drawables [this drawables gl])

  (start-frame [this gl])

  (end-frame [this gl])

  (delete [this gl]))

(defrecord NanoVGRenderer [nanovg]
  Renderer
  (can-draw? [this drawable]
    (satisfies? drawable/NanoVGDrawable drawable))

  (draw-drawables [this drawables gl]
    (let [{:keys [width height]} (opengl/size gl)]
      (NanoVG/beginFrame nanovg width height)
      (doseq [drawable drawables]
        (NanoVG/resetTransform nanovg)
        (NanoVG/translate nanovg
                          (:x drawable)
                          (:y drawable))
        (drawable/draw-nanovg drawable nanovg))
      (NanoVG/endFrame nanovg))
    this)

  (start-frame [this gl] this)

  (end-frame [this gl] this)

  (delete [this gl] this))

(defn create-nanovg-renderer []
  (->NanoVGRenderer (NanoVG/init)))

(defrecord QuadViewRenderer [quad-view]
  Renderer
  (can-draw? [this drawable]
    (satisfies? drawable/Java2DDrawable drawable))

  (draw-drawables [this drawables gl]
    (doto gl
      (.glEnable GL2/GL_BLEND)
      (.glBlendFunc GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA))
    (let [{:keys [width height]} (opengl/size gl)]
      (assoc this :quad-view (quad-view/draw-drawables quad-view drawables width height gl))))

  (start-frame [this gl]
    this)

  (end-frame [this gl]
    (assoc this :quad-view (quad-view/unload-unused-textures quad-view)))

  (delete [this gl]
    this))

(defn create-quad-view-renderer [gl]
  (->QuadViewRenderer (quad-view/create gl)))

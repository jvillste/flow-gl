(ns fungl.root-renderer
  (:require [flow-gl.opengl.jogl.opengl :as opengl]
            [flow-gl.gui.quad-renderer :as quad-renderer]
            [fungl.atom-registry :as atom-registry]))

(defn root-renderer [scene-graph gl]
  (opengl/clear gl 0 0 0 1)
  (let [quad-renderer-atom (atom-registry/get! ::root-renderer (quad-renderer/atom-specification gl))]
    (quad-renderer/render quad-renderer-atom gl scene-graph)))

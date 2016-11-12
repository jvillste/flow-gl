(ns flow-gl.gui.render-target-renderer
  (:require (flow-gl.opengl.jogl [render-target :as render-target])
            (flow-gl.gui [stateful :as stateful])
            (fungl [renderer :as renderer])))

(defn initialize-state [gl]
  {})

(defn quad [scene-graph render-target]
  (assoc (select-keys scene-graph [:x :y :width :height])
         :texture-id (:texture render-target)
         :texture-hash (hash scene-graph)))

(defn render [renderers state-atom gl scene-graph]
  (let [{:keys [render-target previous-scene-graph]} @state-atom]
    (if (= scene-graph
           previous-scene-graph)
      (quad scene-graph
            render-target)
      (let [render-target (if (and render-target
                                   (= (:width render-target)
                                      (:width scene-graph))
                                   (= (:height render-target)
                                      (:height scene-graph)))
                            render-target
                            (do (when render-target
                                  (render-target/delete render-target gl))
                                
                                (render-target/create (:width scene-graph)
                                                      (:height scene-graph)
                                                      gl)))]
        
        (render-target/render-to render-target gl
                                 (renderer/apply-renderers! (assoc scene-graph
                                                                   :renderers renderers
                                                                   :x 0
                                                                   :y 0)
                                                            gl))

        (swap! state-atom assoc
               :render-target render-target
               :previous-scene-graph scene-graph)
        
        (quad scene-graph
              render-target)))))

(defn renderer [& renderers]
  {:initialize-state initialize-state
   :render (partial render renderers)
   :delete-state (fn [state gl] (render-target/delete (:render-target state) gl))})

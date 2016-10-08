(ns fungl.application
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            (flow-gl.gui [window :as window]
                         [layouts :as layouts]
                         [layout :as layout]
                         [quad-renderer :as quad-renderer]
                         [scene-graph :as scene-graph]
                         [mouse :as mouse]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [events :as events]
                         [component :as component])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target])))

(defn start-window [create-scene-graph]
  (let [window-width 400
        window-height 400
        window (jogl-window/create window-width
                                   window-height
                                   :close-automatically true)
        event-channel (window/event-channel window)]

    (keyboard/with-keyboard-state (keyboard/initialize-keybaord-state)
      (component/with-component-state (component/initialize-state)
        (loop [flow-gl-state {:quad-renderer (window/with-gl window gl (quad-renderer/create gl))
                              :mouse-over-state {}}]
          
          (when (window/visible? window)
            (recur (try
                     (let [window-width (window/width window)
                           window-height (window/height window)
                           scene-graph (create-scene-graph)
                           quad-renderer (window/with-gl window gl
                                           (opengl/clear gl 0 0 0 1)

                                           (quad-renderer/draw (:quad-renderer flow-gl-state)
                                                               (scene-graph/leave-nodes scene-graph)
                                                               window-width
                                                               window-height
                                                               gl))]
                       (window/swap-buffers window)

                       (let [event (async/<!! event-channel)
                             mouse-over-state (if (= :mouse (:source event))
                                                (let [mouse-event-handler-nodes-under-mouse (mouse/mouse-event-handler-nodes-in-coodriantes scene-graph
                                                                                                                                            (:x event)
                                                                                                                                            (:y event))]

                                                  (mouse/call-mouse-event-handlers mouse-event-handler-nodes-under-mouse
                                                                                   event)
                                                  (mouse/send-mouse-over-events (:mouse-over-state flow-gl-state)
                                                                                mouse-event-handler-nodes-under-mouse))
                                                (:mouse-over-state flow-gl-state))]
                         (when (= :keyboard
                                  (:source event))
                           (handle-keyboard-event scene-graph event))
                         
                         {:quad-renderer quad-renderer
                          :mouse-over-state mouse-over-state}))

                     
                     
                     (catch Throwable e
                       (.printStackTrace e *out*)
                       (window/close window)
                       (throw e))))))))
    
    
    (println "exiting")))

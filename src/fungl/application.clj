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
                         [component :as component]
                         [stateful :as stateful])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target])))

(defn do-layout [scene-graph window-width window-height]
  (-> scene-graph
      (assoc :x 0
             :y 0
             :available-width window-width
             :available-height window-height)
      (layout/do-layout)))

(defn create-dynamic-state [gl]
  {#'mouse/state-atom (mouse/initialize-state)
   #'keyboard/state-atom (keyboard/initialize-state)
   ;;   #'component/state-atom (component/initialize-state)
   #'stateful/state-atom (stateful/initialize-state)
   #'quad-renderer/state-atom (quad-renderer/initialize-state gl)})

(defn render [gl scene-graph]
  (opengl/clear gl 0 0 0 1)

  (let [{:keys [width height]} (opengl/size gl)]
    (quad-renderer/draw! (scene-graph/leave-nodes scene-graph)
                         width height
                         gl)))

(defn handle-event [scene-graph event]
  (when (= :mouse
           (:source event))
    (mouse/handle-mouse-event! scene-graph event))
  
  (when (= :keyboard
           (:source event))
    (keyboard/handle-keyboard-event! scene-graph event)))

(defn create-window []
  (jogl-window/create 400 400
                      :close-automatically true))


(defn start-window [create-scene-graph & {:keys [window handle-event render create-dynamic-state do-layout]
                                          :or {window (create-window)
                                               handle-event handle-event
                                               render render
                                               create-dynamic-state create-dynamic-state
                                               do-layout do-layout}}]
  (let [event-channel (window/event-channel window)]

    (with-bindings (window/with-gl window gl (create-dynamic-state gl))
      
      (while (window/visible? window)
        (try
          (let [scene-graph (create-scene-graph (window/width window)
                                                (window/height window))]

            (window/with-gl window gl
              (render gl scene-graph))
            
            (window/swap-buffers window)

            (let [event (async/<!! event-channel)]
              (handle-event scene-graph event)))
          
          (catch Throwable e
            (.printStackTrace e *out*)
            (window/close window)
            (throw e)))))    
    
    
    (println "exiting")))


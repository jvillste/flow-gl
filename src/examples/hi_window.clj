(ns examples.render-target
  (:require [flow-gl.opengl.jogl.window :as jogl-window]
            [flow-gl.opengl.jogl.opengl :as opengl]
            [flow-gl.gui.window :as window]
            [clojure.core.async :as async]))

(let [window (jogl-window/create 200
                                 200)
      event-channel (window/event-channel window)]

  (loop [red true]
    (window/with-gl window gl
      (if red
        (opengl/clear gl 1 0 0 1)
        (opengl/clear gl 0 0 1 1)))
    
    (window/swap-buffers window)
    
    (let [event (async/<!! event-channel)]
      (prn event)
      
      (if (or (= (:key event) :esc)
              (= (:type event) :close-requested))
        (window/close window)
        (recur (not red))))))

(ns flow-gl.opengl.jogl.window
  (:require [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.gui.events :as events])
  (:import [com.jogamp.newt.event WindowAdapter WindowEvent]
           [com.jogamp.newt.opengl GLWindow]
           [javax.media.opengl GLCapabilities GLProfile GLContext GL GL2 DebugGL2 GLEventListener GLAutoDrawable]
           [com.jogamp.newt.event KeyAdapter]))

(defn create-keyboard-event [event type]
  (events/create-keyboard-event type
                                (.getKeyCode event)
                                (if (.isPrintableKey event)
                                  (.getKeyChar event)
                                  nil)
                                (.getWhen event)))

(defn create [width height event-queue]
  (let [gl-profile (GLProfile/getDefault)
        gl-capabilities (GLCapabilities. gl-profile)
        window (GLWindow/create gl-capabilities)]
    (.addKeyListener window (proxy [KeyAdapter] []
                              (keyPressed [event]
                                (event-queue/add-event event-queue (create-keyboard-event event :key-pressed)))
                              (keyReleased [event]
                                (event-queue/add-event event-queue (create-keyboard-event event :key-released)))))

    (.setSize window 300 300)
    (.setVisible window true)
    (.addWindowListener window (proxy [WindowAdapter] []
                                 (windowDestroyNotify [event]
                                   (event-queue/add-event event-queue
                                                          (events/create-close-requested-event)))
                                 (windowResized [event]
                                   (event-queue/add-event event-queue
                                                          (events/create-resize-requested-event (.getWidth window)
                                                                                                (.getHeight window))))))
    {:gl-window  window
     :context (.createContext window nil)}))

(defn close [window]
  (.destroy (:gl-window window)))

(defn start-rendering [window]
  (.makeCurrent (:context window))
  (.setGL (:context window) (DebugGL2. (.getGL2 (.getGL (:context window))))))

(defn end-rendering [window]
  (.release (:context window))
  (.swapBuffers (:gl-window window)))




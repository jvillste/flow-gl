(ns flow-gl.opengl.jogl.jogl
  (:import [com.jogamp.newt.event WindowAdapter WindowEvent]
           [com.jogamp.newt.opengl GLWindow]
           [javax.media.opengl GLCapabilities GLProfile GLContext GL GL2 DebugGL2 GLEventListener GLAutoDrawable]
           [com.jogamp.newt.event KeyAdapter]))

(defn create [width height]
  (let [gl-profile (GLProfile/getDefault)
      gl-capabilities (GLCapabilities. gl-profile)
      window (GLWindow/create gl-capabilities)]
  (.addKeyListener window (proxy [KeyAdapter] []
                            (keyPressed [event]
                              (println "key " (.getKeyCode event)))))

  (.setSize window 300 300)
  (.setVisible window true)
  #_(.addWindowListener window (proxy [WindowAdapter] []
                                 (windowDestroyNotify [e]
                                   (println "close"))))

))

(ns flow-gl.opengl.jogl.jogl
  (:import [com.jogamp.newt.event WindowAdapter WindowEvent]
           [com.jogamp.newt.opengl GLWindow]
           [javax.media.opengl GLCapabilities GLProfile GLContext GL GL2 DebugGL2 GLEventListener GLAutoDrawable]
           [com.jogamp.newt.event KeyAdapter]))

(defn resize [gl width height]
  (.glViewport gl 0 0 width height)
  (.glMatrixMode gl  GL2/GL_PROJECTION)
  (.glLoadIdentity gl)
  (.glOrtho gl 0, width, 0, height, -1, 1)

  (.glMatrixMode gl GL2/GL_MODELVIEW)
  (.glLoadIdentity gl)
  (.glScalef gl 1 -1 1)
  (.glTranslatef gl 0 (- height) 0))


(defn initialize-gl [gl]
  (.glEnable gl GL2/GL_BLEND)
  (.glEnable gl GL2/GL_TEXTURE_2D)
  (.glColorMask gl true, true, true, true)
  (.glBlendFunc gl GL2/GL_SRC_ALPHA GL2/GL_ONE_MINUS_SRC_ALPHA))

(defn clear [gl r g b a]
  (.glClearColor gl r g b a)
  (.glClear gl GL2/GL_COLOR_BUFFER_BIT))

(let [gl-profile (GLProfile/getDefault)
      gl-capabilities (GLCapabilities. gl-profile)
      window (GLWindow/create gl-capabilities)]
  (.addKeyListener window (proxy [KeyAdapter] []
                            (keyPressed [event]
                              (println "key " (.getKeyCode event)))))
  (.addGLEventListener window
                       (proxy [GLEventListener] []

                         (reshape  [#^GLAutoDrawable drawable x y w h]
                           (println "reshape")
                           (let [width (.getWidth drawable)
                                 height (.getHeight drawable)]
                             (doto (.getGL drawable)
                               (.glViewport 0 0 width height )
                               (.glMatrixMode GL2/GL_PROJECTION )
                               (.glLoadIdentity)
                               (.glOrtho 0 width height 0 0 1)
                               (.glMatrixMode GL2/GL_MODELVIEW))))

                         (init  [#^GLAutoDrawable drawable]
                           (println "init")
                           (doto (.getGL drawable)
                             (.glClearColor 1.0 1.0 1.0 1.0)
                             (.glColor3f 1.0 0.0 0.0 )
                             (.glPointSize 4.0)

                             (.glDisable GL/GL_DEPTH_TEST)

                             (.glEnable GL/GL_LINE_SMOOTH)
                             (.glEnable GL/GL_BLEND)
                             (.glBlendFunc GL/GL_SRC_ALPHA GL/GL_ONE_MINUS_SRC_ALPHA)
                             (.glHint GL/GL_LINE_SMOOTH_HINT GL/GL_DONT_CARE)))


                         (dispose [#^GLAutoDrawable drawable])

                         (display  [#^GLAutoDrawable drawable]

                           (let [width (.getWidth drawable)
                                 height (.getHeight drawable)]
                             (println (str "display " width " " height ))
                             (doto (.getGL drawable)
                               (.glClear GL/GL_COLOR_BUFFER_BIT)
                               (.glLoadIdentity)
                               (.glColor3f 1.0 0.0 0.0)

                               (.glBegin GL/GL_TRIANGLES)
                               (.glVertex2f 0 0)
                               (.glVertex2f width 0)
                               (.glVertex2f (/ width 2)  height)
                               (.glEnd)

                               (.glColor3f 0.0 1.0 0.0)
                               (.glBegin GL/GL_LINES)
                               (.glVertex2f 0 0)
                               (.glVertex2f width height)
                               (.glEnd))))))
  (.setSize window 300 300)
  (.setVisible window true)
  #_(.addWindowListener window (proxy [WindowAdapter] []
                                 (windowDestroyNotify [e]
                                   (println "close"))))



  #_(let [gl-context (.createContext window nil)]
      (when (not (= (.makeCurrent gl-context)
                    GLContext/CONTEXT_NOT_CURRENT))

        (let [gl2 (.setGL gl-context (DebugGL2. (.getGL2 (.getGL gl-context))))]
          (try
            (initialize-gl gl2)

            (resize gl2 300 300)

            (clear gl2 1 0 0 0)

            (.glBegin gl2 GL/GL_TRIANGLES)

            (.glColor3f gl2 1 0 0)
            (.glVertex2f gl2 0 0)

            (.glColor3f gl2 0 1 0)
            (.glVertex2f gl2 100 100)

            (.glColor3f gl2 0 0 1)
            (.glVertex2f gl2 0 100)

            (.glEnd gl2)

            (.swapBuffers window)
            (catch Exception e
              (.destroy window)
              (throw e))

            (finally (.release gl-context)))))))

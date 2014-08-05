(ns flow-gl.opengl.jogl.window
  (:require [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.gui.events :as events]
            [clojure.core.async :as async])
  (:import [com.jogamp.newt.event WindowAdapter WindowEvent KeyAdapter KeyEvent MouseAdapter MouseEvent]
           [com.jogamp.newt.opengl GLWindow]
           [javax.media.opengl GLCapabilities GLProfile GLContext GL GL2 DebugGL2 DebugGL3 DebugGL4 GLEventListener GLAutoDrawable TraceGL2]
           [javax.media.nativewindow WindowClosingProtocol$WindowClosingMode]))

(def keyboard-keys {KeyEvent/VK_ENTER :enter
                    KeyEvent/VK_ESCAPE :esc
                    KeyEvent/VK_LEFT :left
                    KeyEvent/VK_RIGHT :right
                    KeyEvent/VK_DOWN :down
                    KeyEvent/VK_UP :up
                    KeyEvent/VK_SPACE :space
                    KeyEvent/VK_BACK_SPACE :back-space
                    KeyEvent/VK_F1 :f1
                    KeyEvent/VK_F2 :f2
                    KeyEvent/VK_TAB :tab})

(def mouse-keys {MouseEvent/BUTTON1 :left-button
                 MouseEvent/BUTTON2 :right-button
                 MouseEvent/BUTTON3 :middle-button})

(defn key-code-to-key [key-code key-map]
  (let [key (key-map key-code)]
    (if key
      key
      :unknown)))

(defn create-keyboard-event [event type]
  (events/create-keyboard-event type
                                (key-code-to-key (.getKeyCode event) keyboard-keys)
                                (if (.isPrintableKey event)
                                  (.getKeyChar event)
                                  nil)
                                (.getWhen event)))

(defn create-mouse-event [event type]
  (events/create-mouse-event type
                             (.getX event)
                             (.getY event)
                             (key-code-to-key (.getButton event) mouse-keys)
                             (.getWhen event)))

(defn get-gl [profile ^javax.media.opengl.GLAutoDrawable drawable]
  (case profile
    :gl2 (DebugGL2. (.getGL2 (.getGL drawable)))
    :gl3 (DebugGL3. (.getGL3 (.getGL drawable)))
    :gl4 (DebugGL4. (.getGL4 (.getGL drawable))))

  #_(TraceGL2. (DebugGL2. (.getGL2 (.getGL drawable)))
               System/err))

(defn create
  ;; ([width height]
  ;;    (create width height identity (fn [gl width height]) nil :gl2))

  ;; ([width height init reshape]
  ;;    (create width height init reshape nil :gl2))

  ;; ([width height init reshape event-channel]
  ;;    (create width height init reshape event-channel :gl2))

  ([width height & {:keys [init reshape event-channel profile] :or {init identity reshape (fn [gl width height]) event-channel nil profile :gl2}}]
     (let [gl-profile (GLProfile/get (case profile
                                       :gl2 GLProfile/GL2
                                       :gl3 GLProfile/GL3
                                       :gl4 GLProfile/GL4))
           gl-capabilities (doto (GLCapabilities. gl-profile)
                             (.setDoubleBuffered true))
           display-atom (atom (fn [gl]))
           window (GLWindow/create gl-capabilities)]


       (when event-channel
         (doto window
           (.addKeyListener (proxy [KeyAdapter] []
                              (keyPressed [event]
                                (async/put! event-channel (create-keyboard-event event :key-pressed)))
                              (keyReleased [event]
                                (async/put! event-channel (create-keyboard-event event :key-released)))))

           (.addMouseListener (proxy [MouseAdapter] []
                                (mouseMoved [event]
                                  (async/put! event-channel (create-mouse-event event :mouse-moved)))
                                (mouseDragged [event]
                                  (async/put! event-channel (create-mouse-event event :mouse-dragged)))
                                (mousePressed [event]
                                  (async/put! event-channel (create-mouse-event event :mouse-pressed)))
                                (mouseReleased [event]
                                  (async/put! event-channel (create-mouse-event event :mouse-released)))
                                (mouseClicked [event]
                                  (async/put! event-channel (create-mouse-event event :mouse-clicked)))))

           (.addWindowListener (proxy [WindowAdapter] []
                                 (windowDestroyNotify [event]
                                   (async/put! event-channel
                                               (events/create-close-requested-event)))
                                 (windowResized [event]
                                   #_(flow-gl.debug/debug-timed "window resized"
                                                                (.getWidth window)
                                                                (.getHeight window))

                                   #_(async/go (async/>! event-channel
                                                         (events/create-resize-requested-event (.getWidth window)
                                                                                               (.getHeight window)))))))))

       (doto window
         (.addGLEventListener (proxy [GLEventListener] []
                                (display [^javax.media.opengl.GLAutoDrawable drawable]
                                  #_(flow-gl.debug/debug-timed "display")
                                  (let [gl (get-gl profile drawable)]
                                    #_(doto (get-gl :gl3 drawable)
                                      (.glClearColor 1 0 0 1)
                                      (.glClear GL2/GL_COLOR_BUFFER_BIT))
                                    
                                    (when @display-atom
                                      (@display-atom gl))

                                    #_(when (not @display-atom)
                                        (flow-gl.debug/debug :all "display called without atom" (.getId (java.lang.Thread/currentThread)) (java.util.Date.)))
                                    #_(reset! display-atom nil)))

                                (init [^javax.media.opengl.GLAutoDrawable drawable]
                                  (let [gl (get-gl profile drawable)]
                                    (init gl)))

                                (reshape [^javax.media.opengl.GLAutoDrawable drawable x y width height]
                                  #_(doto (get-gl :gl3 drawable)
                                      (.glClearColor 1 0 0 1)
                                      (.glClear GL2/GL_COLOR_BUFFER_BIT))
                                  #_(.swapBuffers drawable)

                                  (flow-gl.debug/debug-timed "reshape" width height)

                                  #_(async/go (async/>! event-channel
                                                        (events/create-resize-requested-event (.getWidth window)
                                                                                              (.getHeight window))))
                                  #_(async/>!!  event-channel
                                                (events/create-resize-requested-event (.getWidth window)
                                                                                      (.getHeight window)))
                                  #_(flow-gl.debug/debug-timed "")
                                  #_(flow-gl.debug/debug-timed "")


                                  #_(Thread/sleep 500)
                                  #_(println "reshape ready")
                                  #_(.swapBuffers drawable)

                                  #_(let [gl (get-gl profile drawable)]
                                      (reshape gl width height)))

                                (dispose [drawable])
                                (displayChanged [drawable mode-changed device-changed])))

         (.setDefaultCloseOperation WindowClosingProtocol$WindowClosingMode/DO_NOTHING_ON_CLOSE)
         (.setAutoSwapBufferMode true)

         (.setSize width height)
         (.setVisible true))

       {:gl-window  window
        :display-atom display-atom
        :profile profile
        :event-channel event-channel})))

(defn width [window]
  (.getWidth (:gl-window window)))

(defn height [window]
  (.getHeight (:gl-window window)))

(defn close [window]
  (.destroy (:gl-window window)))

(defn render* [window renderer]
  (reset! (:display-atom window) renderer)
  (.display (:gl-window window)))

(defn swap-buffers [window]
  (flow-gl.debug/debug-timed "swap buffers")
  (.swapBuffers (:gl-window window)))

(defmacro render [window gl & body]
  `(render* ~window (fn [~gl] ~@body)))

(defmacro set-display [window gl & body]
  (flow-gl.debug/debug-timed "with-gl")
  `(let [value-atom# (atom {})]
     (render* ~window
              (fn [~gl]
                (reset! value-atom#
                        (do ~@body))))
     @value-atom#))

(defmacro with-gl [window gl & body]
  `(let [value-atom# (atom {})]
     (render* ~window
              (fn [~gl]
                (reset! value-atom#
                        (do ~@body))))
     (reset! (:display-atom ~window) nil)
     @value-atom#))

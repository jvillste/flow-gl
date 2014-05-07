(ns flow-gl.opengl.jogl.window
  (:require [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.gui.events :as events])
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

(defn gl-event-listener [profile init reshape display-atom]
  (proxy [GLEventListener] []
    (display [^javax.media.opengl.GLAutoDrawable drawable]
      (let [gl (get-gl profile drawable)]
        (@display-atom gl)))

    (init [^javax.media.opengl.GLAutoDrawable drawable]
      (let [gl (get-gl profile drawable)]
        (init gl)))

    (reshape [^javax.media.opengl.GLAutoDrawable drawable x y width height]
      (let [gl (get-gl profile drawable)]
        (reshape gl width height)))

    (dispose [drawable])
    (displayChanged [drawable mode-changed device-changed])))

(defn create
  ;; ([width height]
  ;;    (create width height identity (fn [gl width height]) nil :gl2))

  ;; ([width height init reshape]
  ;;    (create width height init reshape nil :gl2))

  ;; ([width height init reshape event-queue]
  ;;    (create width height init reshape event-queue :gl2))

  ([width height & {:keys [init reshape event-queue profile] :or {init identity reshape (fn [gl width height]) event-queue nil profile :gl2}}]
     (let [gl-profile (GLProfile/get (case profile
                                       :gl2 GLProfile/GL2
                                       :gl3 GLProfile/GL3
                                       :gl4 GLProfile/GL4))
           gl-capabilities (GLCapabilities. gl-profile)
           display-atom (atom (fn [gl]))
           window (GLWindow/create gl-capabilities)]


       (when event-queue
         (doto window
           (.addKeyListener (proxy [KeyAdapter] []
                              (keyPressed [event]
                                (event-queue/add-event event-queue (create-keyboard-event event :key-pressed)))
                              (keyReleased [event]
                                (event-queue/add-event event-queue (create-keyboard-event event :key-released)))))

           (.addMouseListener (proxy [MouseAdapter] []
                                (mouseMoved [event]
                                  ;;(event-queue/add-event event-queue (create-mouse-event event :mouse-moved))
                                  )
                                (mousePressed [event]
                                  (println "mouse pressed")
                                  (event-queue/add-event event-queue (create-mouse-event event :mouse-pressed)))
                                (mouseReleased [event]
                                  (event-queue/add-event event-queue (create-mouse-event event :mouse-released)))
                                (mouseClicked [event]
                                  (println "mouse clicked")
                                  (event-queue/add-event event-queue (create-mouse-event event :mouse-clicked)))))

           (.addWindowListener (proxy [WindowAdapter] []
                                 (windowDestroyNotify [event]
                                   (println "destroy notify")
                                   (event-queue/add-event event-queue
                                                          (events/create-close-requested-event)))
                                 (windowResized [event]
                                   (event-queue/add-event event-queue
                                                          (events/create-resize-requested-event (.getWidth window)
                                                                                                (.getHeight window))))))

           (.setDefaultCloseOperation WindowClosingProtocol$WindowClosingMode/DO_NOTHING_ON_CLOSE)))

       (doto window
         (.addGLEventListener  (gl-event-listener profile init reshape display-atom ))

         (.setSize width height)

         (.setVisible true))


       {:gl-window  window
        :display-atom display-atom
        :profile profile})))

(defn width [window]
  (.getWidth (:gl-window window)))

(defn height [window]
  (.getHeight (:gl-window window)))

(defn close [window]
  (.destroy (:gl-window window)))

(defn render* [window renderer]
  (reset! (:display-atom window) renderer)
  (.display (:gl-window window)))

(defmacro render [window gl & body]
  `(render* ~window (fn [~gl] ~@body)))

(defmacro with-gl [window gl & body]
  `(let [value-atom# (atom {})]
     (render* ~window
              (fn [~gl]
                (reset! value-atom#
                        (do ~@body))))
     @value-atom#))

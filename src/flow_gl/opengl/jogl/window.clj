(ns flow-gl.opengl.jogl.window
  (:require [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.gui.events :as events])
  (:import [com.jogamp.newt.event WindowAdapter WindowEvent KeyAdapter KeyEvent MouseAdapter MouseEvent]
           [com.jogamp.newt.opengl GLWindow]
           [javax.media.opengl GLCapabilities GLProfile GLContext GL GL2 DebugGL2 GLEventListener GLAutoDrawable TraceGL2]
           [javax.media.nativewindow WindowClosingProtocol$WindowClosingMode]))

(def keyboard-keys {KeyEvent/VK_ENTER :enter
                    KeyEvent/VK_ESCAPE :esc
                    KeyEvent/VK_LEFT :left
                    KeyEvent/VK_RIGHT :right
                    KeyEvent/VK_DOWN :down
                    KeyEvent/VK_UP :up
                    KeyEvent/VK_SPACE :space})

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

(defn get-gl [^javax.media.opengl.GLAutoDrawable drawable]
  (DebugGL2. (.getGL2 (.getGL drawable)))
  #_(TraceGL2. (DebugGL2. (.getGL2 (.getGL drawable)))
               System/err))

(defn gl-event-listener [init reshape display-atom]
  (proxy [GLEventListener] []
    (display [^javax.media.opengl.GLAutoDrawable drawable]
      (let [^GL2 gl (get-gl drawable)]
        (@display-atom gl)))

    (init [^javax.media.opengl.GLAutoDrawable drawable]
      (let [gl (get-gl drawable)]
        (init gl)))

    (reshape [^javax.media.opengl.GLAutoDrawable drawable x y width height]
      (let [gl (get-gl drawable)]
        (reshape gl width height)))

    (dispose [drawable])
    (displayChanged [drawable mode-changed device-changed])))

(defn create
  ([width height]
     (create width height identity (fn [gl width height]) nil))

  ([width height init reshape]
     (create width height init reshape nil))

  ([width height init reshape event-queue]
     (let [gl-profile (GLProfile/get GLProfile/GL2)
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
                                   (event-queue/add-event event-queue
                                                          (events/create-close-requested-event)))
                                 (windowResized [event]
                                   (event-queue/add-event event-queue
                                                          (events/create-resize-requested-event (.getWidth window)
                                                                                                (.getHeight window))))))

           (.setDefaultCloseOperation WindowClosingProtocol$WindowClosingMode/DO_NOTHING_ON_CLOSE)))

       (doto window
         (.addGLEventListener  (gl-event-listener init reshape display-atom ))

         (.setSize width height)

         (.setVisible true))


       {:gl-window  window
        :display-atom display-atom})))

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

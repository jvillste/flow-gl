(ns flow-gl.opengl.jogl.window
  (:require [flow-gl.opengl.jogl.opengl :as opengl]
            [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.gui.events :as events]
            [clojure.core.async :as async]
            [flow-gl.gui.window :as window])
  (:import [com.jogamp.newt.event WindowAdapter WindowEvent KeyAdapter KeyEvent MouseAdapter MouseEvent]
           [com.jogamp.newt.opengl GLWindow]
           [com.jogamp.newt.awt NewtCanvasAWT]
           [java.awt Frame BorderLayout]
           [javax.swing SwingUtilities]
           [com.jogamp.opengl GLCapabilities GLProfile GLContext GL GL2 DebugGL2 DebugGL3 DebugGL4 GLEventListener GLAutoDrawable TraceGL2]
           [com.jogamp.nativewindow WindowClosingProtocol$WindowClosingMode]))


(defrecord JoglWindow [gl-window event-channel runner-atom frame]
  window/Window
  (run-with-gl [this runner]
    (reset! runner-atom runner)
    (.display gl-window))
  (swap-buffers [this] (.swapBuffers gl-window))
  (event-channel [this] event-channel)
  (visible? [this] (.isVisible gl-window))
  (width [this] (.getSurfaceWidth gl-window))
  (height [this] (.getSurfaceHeight gl-window))
  (close [this]
    (.destroy gl-window)
    (when frame
      (.dispose frame))))

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
                    KeyEvent/VK_TAB :tab
                    KeyEvent/VK_F :f
                    KeyEvent/VK_N :n
                    KeyEvent/VK_P :p
                    KeyEvent/VK_B :b})

(def mouse-keys {MouseEvent/BUTTON1 :left-button
                 MouseEvent/BUTTON2 :middle-button
                 MouseEvent/BUTTON3 :right-button})

(defn key-code-to-key [key-code key-map]
  (let [key (key-map key-code)]
    (if key
      key
      :unknown)))

(defn create-event [event type]
  {:type type
   :time (.getWhen event)
   :shift (.isShiftDown event)
   :control (.isControlDown event)
   :alt (.isAltDown event)})

(defn create-keyboard-event [event type]
  (events/create-keyboard-event type
                                (key-code-to-key (.getKeyCode event) keyboard-keys)
                                (if (.isPrintableKey event)
                                  (.getKeyChar event)
                                  nil)
                                (.getWhen event)
                                (.isShiftDown event)
                                (.isControlDown event)
                                (.isAltDown event)
                                (.isAutoRepeat event)
                                (.getKeyCode event)))

(defn create-mouse-event [event surface-scale type]
  (conj (create-event event type)
        {:x (* (.getX event) (first surface-scale))
         :y (* (.getY event) (second surface-scale))
         :pressure (.getPressure event true)
         :key (key-code-to-key (.getButton event) mouse-keys)
         :source :mouse}))

(defn get-gl [profile ^com.jogamp.opengl.GLAutoDrawable drawable]
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

  ([width height & {:keys [init reshape event-channel profile close-automatically use-awt awt-init] :or {init opengl/initialize reshape (fn [gl width height]) event-channel (async/chan 50) profile :gl3 close-automatically false use-awt false awt-init nil}}]
   (let [gl-profile (GLProfile/get (case profile
                                     :gl2 GLProfile/GL2
                                     :gl3 GLProfile/GL3
                                     :gl4 GLProfile/GL4))
         gl-capabilities (doto (GLCapabilities. gl-profile)
                           (.setDoubleBuffered true)
                           (.setStencilBits 1))
         runner-atom (atom (fn [gl]))
         window (GLWindow/create gl-capabilities)
         surface-scale (float-array 2)]
     

     #_(prn (.getMaximumSurfaceScale window (float-array 2)))

     (when event-channel
       (doto window
         (.addKeyListener (proxy [KeyAdapter] []
                            (keyPressed [event]
                              (async/put! event-channel (create-keyboard-event event :key-pressed)))
                            (keyReleased [event]
                              (async/put! event-channel (create-keyboard-event event :key-released)))))

         (.addMouseListener (proxy [MouseAdapter] []
                              (mouseMoved [event]
                                (async/put! event-channel (create-mouse-event event surface-scale :mouse-moved)))
                              (mouseDragged [event]
                                (async/put! event-channel (create-mouse-event event surface-scale :mouse-dragged)))
                              (mousePressed [event]
                                (async/put! event-channel (create-mouse-event event surface-scale :mouse-pressed)))
                              (mouseReleased [event]
                                (async/put! event-channel (create-mouse-event event surface-scale :mouse-released)))
                              (mouseClicked [event]
                                (async/put! event-channel (create-mouse-event event surface-scale :mouse-clicked)))
                              (mouseWheelMoved [event]
                                (async/put! event-channel (let [[x-distance y-distance z-distance] (.getRotation event)]
                                                            (assoc (create-mouse-event event surface-scale :mouse-wheel-moved)
                                                                   :x-distance x-distance
                                                                   :y-distance y-distance
                                                                   :z-distance z-distance
                                                                   :rotation-scale (.getRotationScale event)))))))

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
                                                                                             (.getHeight window)))))
                               ))))

     (doto window
       (.addGLEventListener (proxy [GLEventListener] []
                              (display [^com.jogamp.opengl.GLAutoDrawable drawable]

                                (let [gl (get-gl profile drawable)]
                                  #_(flow-gl.debug/debug-timed "display start" (flow-gl.opengl.jogl.opengl/size gl))
                                  #_(doto (get-gl :gl3 drawable)
                                      (.glClearColor 0 1 0 1)
                                      (.glClear GL2/GL_COLOR_BUFFER_BIT))
                                  #_(Thread/sleep 5)
                                  #_(.swapBuffers drawable)
                                  #_(flow-gl.debug/debug-timed "display end" (flow-gl.opengl.jogl.opengl/size gl))

                                  #_(Thread/sleep 1000)

                                  (when @runner-atom
                                    (@runner-atom gl)
                                    (reset! runner-atom nil))

                                  #_(when (not @runner-atom)
                                      (flow-gl.debug/debug :all "display called without atom" (.getId (java.lang.Thread/currentThread)) (java.util.Date.)))
                                  #_(reset! runner-atom nil)))

                              (init [^com.jogamp.opengl.GLAutoDrawable drawable]
                                (let [gl (get-gl profile drawable)]
                                  (init gl)))

                              (reshape [^com.jogamp.opengl.GLAutoDrawable drawable x y width height]
                                #_(let [gl (get-gl profile drawable)]
                                    #_(when @runner-atom
                                        (do (@runner-atom gl)
                                            (.swapBuffers drawable)))

                                    #_(flow-gl.debug/debug-timed "resize start" (flow-gl.opengl.jogl.opengl/size gl))
                                    #_(doto gl
                                        (.glClearColor 1 0 0 1)
                                        (.glClear GL2/GL_COLOR_BUFFER_BIT))
                                    #_(.swapBuffers drawable)
                                    #_(flow-gl.debug/debug-timed "resize end" (flow-gl.opengl.jogl.opengl/size gl)))


                                #_(flow-gl.debug/debug-timed "reshape" width height)

                                (async/go (async/>! event-channel
                                                    (events/create-resize-requested-event width height)))

                                #_(let [gl (get-gl profile drawable)]
                                    (reshape gl width height)))

                              (dispose [drawable])
                              (displayChanged [drawable mode-changed device-changed])))


       (.setAutoSwapBufferMode false))

     (let [frame (if use-awt
                   (let [frame (javax.swing.JFrame.)]
                     (SwingUtilities/invokeAndWait (fn []
                                                     (let [canvas (NewtCanvasAWT. window)]
                                                       (when awt-init
                                                         (awt-init canvas))
                                                       (doto frame
                                                         (.add canvas BorderLayout/CENTER)
                                                         (.addWindowListener (proxy [java.awt.event.WindowAdapter] []
                                                                               (windowClosing [event]
                                                                                 (.dispose frame))))
                                                         (.setSize width height)
                                                         (.setVisible true)))))
                     frame)
                   
                   (do (doto window
                         (.setSize width height)
                         (.setVisible true)
                         (.getCurrentSurfaceScale surface-scale)
                         )
                       nil))]
       
       (when (not close-automatically)
         (.setDefaultCloseOperation window WindowClosingProtocol$WindowClosingMode/DO_NOTHING_ON_CLOSE))
       
       (->JoglWindow window
                     event-channel
                     runner-atom
                     frame)))))


#_(defn start [app]
    (let [window (window/create 300
                                400
                                :profile :gl3
                                :init opengl/initialize
                                :reshape opengl/resize)]

      (try
        (loop [state {}]

          (if (:close-requested state)
            (window/close window)

            (recur (app window
                        state
                        (drain (window/event-channel window)
                               (or (:sleep-time state)
                                   0))))))

        (catch Exception e
          (window/close window)
          (throw e)))))



(defn swap-buffers [window]
  (.swapBuffers (:gl-window window)))

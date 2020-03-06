(ns flow-gl.swing.window
  (:require [clojure.core.async :as async]
            [flow-gl.gui.events :as events]
            [flow-gl.gui.window :as window])
  (:import (java.awt BorderLayout Panel Frame BasicStroke Color RenderingHints)
           (java.awt.event WindowAdapter ComponentAdapter KeyEvent KeyAdapter MouseEvent MouseAdapter)
           (java.awt.geom RoundRectangle2D$Double)
           (javax.swing JFrame JPanel WindowConstants)))

(defrecord SwingWindow [j-frame j-panel event-channel paint-function-atom]
  window/Window
  (run-with-gl [this runner]
    (reset! paint-function-atom runner)
    (.repaint j-panel))
  (swap-buffers [this])
  (event-channel [this] event-channel)
  (visible? [this] (.isVisible j-frame))
  (width [this] (.getWidth (.getSize j-panel)))
  (height [this] (.getHeight (.getSize j-panel)))
  (close [this] (.dispose j-frame)))

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

(defn create-keyboard-event [event type]
  (events/create-keyboard-event type
                                (or (keyboard-keys (.getKeyCode event))
                                    :unknown)
                                (if true #_(.isPrintableKey event)
                                  (.getKeyChar event)
                                  nil)
                                (.getWhen event)
                                (.isShiftDown event)
                                (.isControlDown event)
                                (.isAltDown event)
                                false #_(.isAutoRepeat event)
                                (.getKeyCode event)))

(defn create-mouse-event [event type]
  (conj {:type type
         :time (.getWhen event)
         :shift (.isShiftDown event)
         :control (.isControlDown event)
         :alt (.isAltDown event)
         :x (* (.getX event) 1 #_(first surface-scale))
         :y (* (.getY event) 1 #_(second surface-scale))
        ;; :pressure (.getPressure event true)
         :key (or (keyboard-keys (.getButton event))
                  :unknown)
         :source :mouse}))

(defn create
  ([width height & {:keys [event-channel width height close-automatically?] :or {event-channel (async/chan) width 300 height 300
                                                                                 close-automatically? true}}]
   (let [paint-function-atom (atom (fn [graphics]))
         j-frame (JFrame.)
         j-panel (proxy [JPanel] []
                   (paintComponent [graphics]
                     (proxy-super paintComponent graphics)
                     (@paint-function-atom graphics)))]

     (doto j-frame
       (.addKeyListener (proxy [KeyAdapter] []
                          (keyPressed [event]
                            (async/put! event-channel (create-keyboard-event event :key-pressed)))
                          (keyReleased [event]
                            (async/put! event-channel (create-keyboard-event event :key-released)))
                          (keyTyped [event]
                            (async/put! event-channel (create-keyboard-event event :key-released))))))

     (doto j-panel
       (.addComponentListener (proxy [ComponentAdapter] []
                                (componentResized [component-event]
                                  (async/put! event-channel
                                              (events/create-resize-requested-event (.getWidth (.getSize j-panel))
                                                                                    (.getHeight (.getSize j-panel)))))))

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
                              (async/put! event-channel (create-mouse-event event :mouse-clicked)))
                            (mouseWheelMoved [event]
                              (async/put! event-channel (let [[x-distance y-distance z-distance] (.getRotation event)]
                                                          (assoc (create-mouse-event event :mouse-wheel-moved)
                                                                 :x-distance x-distance
                                                                 :y-distance y-distance
                                                                 :z-distance z-distance
                                                                 :rotation-scale (.getRotationScale event))))))))

     (when (not close-automatically?)
       (.setDefaultCloseOperation WindowConstants/DO_NOTHING_ON_CLOSE))

     (doto j-frame
       (.setContentPane j-panel)
       (.setSize width height)
       (.addWindowListener (proxy [WindowAdapter] []
                             (windowClosing [window-event]
                               (async/put! event-channel
                                           (events/create-close-requested-event))
                               #_(.dispose j-frame))))
       (.setVisible true))


     (->SwingWindow j-frame
                    j-panel
                    event-channel
                    paint-function-atom))))

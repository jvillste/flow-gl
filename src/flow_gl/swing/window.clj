(ns flow-gl.swing.window
  (:require [clojure.core.async :as async]
            [flow-gl.gui.events :as events]
            [flow-gl.gui.window :as window]
            [clojure.reflect :as reflect]
            [clojure.string :as string]
            [flow-gl.graphics.font :as font]
            [clojure.java.io :as io]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle])
  (:import [java.awt.event ComponentAdapter KeyAdapter KeyEvent MouseAdapter MouseEvent MouseMotionAdapter MouseWheelListener WindowAdapter]
           [javax.swing JFrame JPanel]
           [java.awt Cursor Toolkit Point Canvas]
           java.awt.image.BufferedImage
           java.awt.RenderingHints
           java.awt.Font))

(defrecord SwingWindow [j-frame canvas event-channel]
  window/Window
  (run-with-gl [this runner]
    (let [buffer-strategy (.getBufferStrategy canvas)
          graphics (.getDrawGraphics buffer-strategy)]
      (try
        (runner graphics)
        (finally
          (.dispose graphics)
          (.show buffer-strategy)))))
  (swap-buffers [this])
  (event-channel [this] event-channel)
  (visible? [this] (.isVisible j-frame))
  (width [this] (.getWidth (.getSize canvas)))
  (height [this] (.getHeight (.getSize canvas)))
  (close [this] (.dispose j-frame)))

(def keyboard-keys {KeyEvent/VK_0 :0
                    KeyEvent/VK_1 :1
                    KeyEvent/VK_2 :2
                    KeyEvent/VK_3 :3
                    KeyEvent/VK_4 :4
                    KeyEvent/VK_5 :5
                    KeyEvent/VK_6 :6
                    KeyEvent/VK_7 :7
                    KeyEvent/VK_8 :8
                    KeyEvent/VK_9 :9
                    KeyEvent/VK_A :a
                    KeyEvent/VK_ACCEPT :accept
                    KeyEvent/VK_ADD :add
                    KeyEvent/VK_AGAIN :again
                    KeyEvent/VK_ALL_CANDIDATES :all-candidates
                    KeyEvent/VK_ALPHANUMERIC :alphanumeric
                    KeyEvent/VK_ALT :alt
                    KeyEvent/VK_ALT_GRAPH :alt-graph
                    KeyEvent/VK_AMPERSAND :ampersand
                    KeyEvent/VK_ASTERISK :asterisk
                    KeyEvent/VK_AT :at
                    KeyEvent/VK_B :b
                    KeyEvent/VK_BACK_QUOTE :back-quote
                    KeyEvent/VK_BACK_SLASH :back-slash
                    KeyEvent/VK_BACK_SPACE :back-space
                    KeyEvent/VK_BEGIN :begin
                    KeyEvent/VK_BRACELEFT :braceleft
                    KeyEvent/VK_BRACERIGHT :braceright
                    KeyEvent/VK_C :c
                    KeyEvent/VK_CANCEL :cancel
                    KeyEvent/VK_CAPS_LOCK :caps-lock
                    KeyEvent/VK_CIRCUMFLEX :circumflex
                    KeyEvent/VK_CLEAR :clear
                    KeyEvent/VK_CLOSE_BRACKET :close-bracket
                    KeyEvent/VK_CODE_INPUT :code-input
                    KeyEvent/VK_COLON :colon
                    KeyEvent/VK_COMMA :comma
                    KeyEvent/VK_COMPOSE :compose
                    KeyEvent/VK_CONTEXT_MENU :context-menu
                    KeyEvent/VK_CONTROL :control
                    KeyEvent/VK_CONVERT :convert
                    KeyEvent/VK_COPY :copy
                    KeyEvent/VK_CUT :cut
                    KeyEvent/VK_D :d
                    KeyEvent/VK_DEAD_ABOVEDOT :dead-abovedot
                    KeyEvent/VK_DEAD_ABOVERING :dead-abovering
                    KeyEvent/VK_DEAD_ACUTE :dead-acute
                    KeyEvent/VK_DEAD_BREVE :dead-breve
                    KeyEvent/VK_DEAD_CARON :dead-caron
                    KeyEvent/VK_DEAD_CEDILLA :dead-cedilla
                    KeyEvent/VK_DEAD_CIRCUMFLEX :dead-circumflex
                    KeyEvent/VK_DEAD_DIAERESIS :dead-diaeresis
                    KeyEvent/VK_DEAD_DOUBLEACUTE :dead-doubleacute
                    KeyEvent/VK_DEAD_GRAVE :dead-grave
                    KeyEvent/VK_DEAD_IOTA :dead-iota
                    KeyEvent/VK_DEAD_MACRON :dead-macron
                    KeyEvent/VK_DEAD_OGONEK :dead-ogonek
                    KeyEvent/VK_DEAD_SEMIVOICED_SOUND :dead-semivoiced-sound
                    KeyEvent/VK_DEAD_TILDE :dead-tilde
                    KeyEvent/VK_DEAD_VOICED_SOUND :dead-voiced-sound
                    KeyEvent/VK_DECIMAL :decimal
                    KeyEvent/VK_DELETE :delete
                    KeyEvent/VK_DIVIDE :divide
                    KeyEvent/VK_DOLLAR :dollar
                    KeyEvent/VK_DOWN :down
                    KeyEvent/VK_E :e
                    KeyEvent/VK_END :end
                    KeyEvent/VK_ENTER :enter
                    KeyEvent/VK_EQUALS :equals
                    KeyEvent/VK_ESCAPE :escape
                    KeyEvent/VK_EURO_SIGN :euro-sign
                    KeyEvent/VK_EXCLAMATION_MARK :exclamation-mark
                    KeyEvent/VK_F :f
                    KeyEvent/VK_F1 :f1
                    KeyEvent/VK_F10 :f10
                    KeyEvent/VK_F11 :f11
                    KeyEvent/VK_F12 :f12
                    KeyEvent/VK_F13 :f13
                    KeyEvent/VK_F14 :f14
                    KeyEvent/VK_F15 :f15
                    KeyEvent/VK_F16 :f16
                    KeyEvent/VK_F17 :f17
                    KeyEvent/VK_F18 :f18
                    KeyEvent/VK_F19 :f19
                    KeyEvent/VK_F2 :f2
                    KeyEvent/VK_F20 :f20
                    KeyEvent/VK_F21 :f21
                    KeyEvent/VK_F22 :f22
                    KeyEvent/VK_F23 :f23
                    KeyEvent/VK_F24 :f24
                    KeyEvent/VK_F3 :f3
                    KeyEvent/VK_F4 :f4
                    KeyEvent/VK_F5 :f5
                    KeyEvent/VK_F6 :f6
                    KeyEvent/VK_F7 :f7
                    KeyEvent/VK_F8 :f8
                    KeyEvent/VK_F9 :f9
                    KeyEvent/VK_FINAL :final
                    KeyEvent/VK_FIND :find
                    KeyEvent/VK_FULL_WIDTH :full-width
                    KeyEvent/VK_G :g
                    KeyEvent/VK_GREATER :greater
                    KeyEvent/VK_H :h
                    KeyEvent/VK_HALF_WIDTH :half-width
                    KeyEvent/VK_HELP :help
                    KeyEvent/VK_HIRAGANA :hiragana
                    KeyEvent/VK_HOME :home
                    KeyEvent/VK_I :i
                    KeyEvent/VK_INPUT_METHOD_ON_OFF :input-method-on-off
                    KeyEvent/VK_INSERT :insert
                    KeyEvent/VK_INVERTED_EXCLAMATION_MARK :inverted-exclamation-mark
                    KeyEvent/VK_J :j
                    KeyEvent/VK_JAPANESE_HIRAGANA :japanese-hiragana
                    KeyEvent/VK_JAPANESE_KATAKANA :japanese-katakana
                    KeyEvent/VK_JAPANESE_ROMAN :japanese-roman
                    KeyEvent/VK_K :k
                    KeyEvent/VK_KANA :kana
                    KeyEvent/VK_KANA_LOCK :kana-lock
                    KeyEvent/VK_KANJI :kanji
                    KeyEvent/VK_KATAKANA :katakana
                    KeyEvent/VK_KP_DOWN :kp-down
                    KeyEvent/VK_KP_LEFT :kp-left
                    KeyEvent/VK_KP_RIGHT :kp-right
                    KeyEvent/VK_KP_UP :kp-up
                    KeyEvent/VK_L :l
                    KeyEvent/VK_LEFT :left
                    KeyEvent/VK_LEFT_PARENTHESIS :left-parenthesis
                    KeyEvent/VK_LESS :less
                    KeyEvent/VK_M :m
                    KeyEvent/VK_META :meta
                    KeyEvent/VK_MINUS :minus
                    KeyEvent/VK_MODECHANGE :modechange
                    KeyEvent/VK_MULTIPLY :multiply
                    KeyEvent/VK_N :n
                    KeyEvent/VK_NONCONVERT :nonconvert
                    KeyEvent/VK_NUMBER_SIGN :number-sign
                    KeyEvent/VK_NUMPAD0 :numpad0
                    KeyEvent/VK_NUMPAD1 :numpad1
                    KeyEvent/VK_NUMPAD2 :numpad2
                    KeyEvent/VK_NUMPAD3 :numpad3
                    KeyEvent/VK_NUMPAD4 :numpad4
                    KeyEvent/VK_NUMPAD5 :numpad5
                    KeyEvent/VK_NUMPAD6 :numpad6
                    KeyEvent/VK_NUMPAD7 :numpad7
                    KeyEvent/VK_NUMPAD8 :numpad8
                    KeyEvent/VK_NUMPAD9 :numpad9
                    KeyEvent/VK_NUM_LOCK :num-lock
                    KeyEvent/VK_O :o
                    KeyEvent/VK_OPEN_BRACKET :open-bracket
                    KeyEvent/VK_P :p
                    KeyEvent/VK_PAGE_DOWN :page-down
                    KeyEvent/VK_PAGE_UP :page-up
                    KeyEvent/VK_PASTE :paste
                    KeyEvent/VK_PAUSE :pause
                    KeyEvent/VK_PERIOD :period
                    KeyEvent/VK_PLUS :plus
                    KeyEvent/VK_PREVIOUS_CANDIDATE :previous-candidate
                    KeyEvent/VK_PRINTSCREEN :printscreen
                    KeyEvent/VK_PROPS :props
                    KeyEvent/VK_Q :q
                    KeyEvent/VK_QUOTE :quote
                    KeyEvent/VK_QUOTEDBL :quotedbl
                    KeyEvent/VK_R :r
                    KeyEvent/VK_RIGHT :right
                    KeyEvent/VK_RIGHT_PARENTHESIS :right-parenthesis
                    KeyEvent/VK_ROMAN_CHARACTERS :roman-characters
                    KeyEvent/VK_S :s
                    KeyEvent/VK_SCROLL_LOCK :scroll-lock
                    KeyEvent/VK_SEMICOLON :semicolon
;;                    KeyEvent/VK_SEPARATER :separater
                    KeyEvent/VK_SEPARATOR :separator
                    KeyEvent/VK_SHIFT :shift
                    KeyEvent/VK_SLASH :slash
                    KeyEvent/VK_SPACE :space
                    KeyEvent/VK_STOP :stop
                    KeyEvent/VK_SUBTRACT :subtract
                    KeyEvent/VK_T :t
                    KeyEvent/VK_TAB :tab
                    KeyEvent/VK_U :u
                    KeyEvent/VK_UNDEFINED :undefined
                    KeyEvent/VK_UNDERSCORE :underscore
                    KeyEvent/VK_UNDO :undo
                    KeyEvent/VK_UP :up
                    KeyEvent/VK_V :v
                    KeyEvent/VK_W :w
                    KeyEvent/VK_WINDOWS :windows
                    KeyEvent/VK_X :x
                    KeyEvent/VK_Y :y
                    KeyEvent/VK_Z :z})
(comment
  (doseq [name (->> (:members (reflect/reflect KeyEvent))
                    (map :name)
                    (map name)
                    (filter (fn [name]
                              (string/starts-with? name
                                                   "VK_")))
                    (sort))]
    (println (str "KeyEvent/" name " :" (-> (subs (string/lower-case name)
                                                  3)
                                            (string/replace "_" "-")))))
  )


(def mouse-keys {MouseEvent/BUTTON1 :left-button
                 MouseEvent/BUTTON2 :middle-button
                 MouseEvent/BUTTON3 :right-button})

(defn create-keyboard-event [event type]
  (events/create-keyboard-event type
                                (or (keyboard-keys (.getKeyCode event))
                                    (when-let [character (.getKeyChar event)]
                                      (keyword character))
                                    :unknown)
                                (.getKeyChar event)
                                (.getWhen event)
                                (.isShiftDown event)
                                (.isControlDown event)
                                (.isAltDown event)
                                nil #_(.isAutoRepeat event)
                                (.getKeyCode event)))

(defn create-mouse-event [event type]
  (conj {:type type
         :time (.getWhen event)
         :shift (.isShiftDown event)
         :control (.isControlDown event)
         :alt (.isAltDown event)
         :x (* (.getX event) 2 #_(first surface-scale))
         :y (* (.getY event) 2 #_(second surface-scale))
         ;; :pressure (.getPressure event true)
         :key (or (keyboard-keys (.getButton event))
                  :unknown)
         :source :mouse}))

(defn create ([width height & {:keys [event-channel close-automatically?] :or {event-channel (async/chan)
                                                                               close-automatically? true}}]

              (let [j-frame (JFrame.)
                    canvas (Canvas.)]

                (.add (.getContentPane j-frame)
                      canvas)

                (doto j-frame
                  (.pack)
                  (.setSize width height)
                  (.setVisible true))

                (.setIgnoreRepaint canvas true)
                (.createBufferStrategy canvas 2)

                (doto j-frame
                  #_(.addKeyListener (proxy [KeyAdapter] []
                                       (keyPressed [event]
                                         (async/put! event-channel (create-keyboard-event event :key-pressed)))
                                       (keyReleased [event]
                                         (async/put! event-channel (create-keyboard-event event :key-released)))
                                       (keyTyped [event]
                                         (async/put! event-channel (create-keyboard-event event :key-typed)))))

                  (.addWindowListener (proxy [WindowAdapter] []
                                        (windowClosing [event]
                                          (async/put! event-channel (events/create-close-requested-event)))))
                  #_(.addMouseMotionListener (proxy [MouseMotionAdapter] []
                                               (mouseMoved [event]
                                                 (async/put! event-channel (create-mouse-event event :mouse-moved)))
                                               (mouseDragged [event]
                                                 (async/put! event-channel (create-mouse-event event :mouse-dragged))))))

                (doto canvas
                  (.setFocusTraversalKeysEnabled false)
                  (.addKeyListener (proxy [KeyAdapter] []
                                     (keyPressed [event]
                                       (async/put! event-channel (create-keyboard-event event :key-pressed)))
                                     (keyReleased [event]
                                       (async/put! event-channel (create-keyboard-event event :key-released)))
                                     (keyTyped [event]
                                       (async/put! event-channel (create-keyboard-event event :key-typed)))))
                  (.addMouseMotionListener (proxy [MouseMotionAdapter] []
                                             (mouseMoved [event]
                                               (async/put! event-channel (create-mouse-event event :mouse-moved)))
                                             (mouseDragged [event]
                                               (async/put! event-channel (create-mouse-event event :mouse-moved)))))

                  #_(.setOpaque true)
                  (.addComponentListener (proxy [ComponentAdapter] []
                                           (componentResized [component-event]
                                             (async/put! event-channel
                                                         (events/create-resize-requested-event (* 2 (.getWidth (.getSize canvas)))
                                                                                               (* 2 (.getHeight (.getSize canvas))))))))

                  #_(.addMouseMotionListener (proxy [MouseMotionAdapter] []
                                               (mouseMoved [event]
                                                 (prn 'mouseMoved)
                                                 (async/put! event-channel (create-mouse-event event :mouse-moved)))
                                               (mouseDragged [event]
                                                 #_(async/put! event-channel (create-mouse-event event :mouse-dragged)))))

                  (.addMouseListener (proxy [MouseAdapter] []
                                       (mousePressed [event]
                                         (async/put! event-channel (create-mouse-event event :mouse-pressed)))
                                       (mouseReleased [event]
                                         (async/put! event-channel (create-mouse-event event :mouse-released)))
                                       (mouseClicked [event]
                                         (async/put! event-channel (create-mouse-event event :mouse-clicked)))))

                  (.addMouseWheelListener (proxy [MouseWheelListener] []
                                            (mouseWheelMoved [event]
                                              #_(async/put! event-channel (let [[x-distance y-distance z-distance] (.getRotation event)]
                                                                            (assoc (create-mouse-event event :mouse-wheel-moved)
                                                                                   :x-distance x-distance
                                                                                   :y-distance y-distance
                                                                                   :z-distance z-distance
                                                                                   :rotation-scale (.getRotationScale event)))))))
                  (.requestFocus))

                (->SwingWindow j-frame
                               canvas
                               event-channel))))

(defn create-cursor [buffered-image]
  (.createCustomCursor (Toolkit/getDefaultToolkit)
                       buffered-image
                       (Point. 0 0)
                       "cursor"))

(defn set-cursor [window cursor]
  (.setCursor (.getContentPane (:j-frame window))
              cursor))

(ns flow-gl.opengl.window
  (:require (flow-gl [debug :as debug])
            [flow-gl.gui.awt-input :as awt-input])
  (:import [org.lwjgl.opengl GLContext PixelFormat Display DisplayMode GL11 GL12  GL30 ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.awt Frame Canvas BorderLayout Button]
           [java.awt.event WindowAdapter ComponentAdapter KeyAdapter]
           [java.nio IntBuffer FloatBuffer]))

(defrecord Window [frame
                   close-requested
                   resize-requested
                   width
                   height])

(defn resize [width height]
  (GL11/glViewport 0 0 width height)
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glLoadIdentity)
  (GL11/glOrtho 0, width, 0, height, -1, 1)

  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)
  (GL11/glScalef 1 -1 1)
  (GL11/glTranslatef 0 (- height) 0))

(defn swap-buffers []
  (Display/update false))

(defn wait-for-next-frame [framerate]
  (Display/sync framerate))

(defn update [window framerate]
  (Display/update false)
  (Display/sync framerate))

(defn initialize-fps [window]
  (assoc window
    :last-fps-time (System/nanoTime)
    :frames-since-last-update 1))

(defn show-fps [window]
  (let [last-time (:last-fps-time window)
        frames-since-last-update (:frames-since-last-update window)
        time-now (System/nanoTime)
        difference (/ (- time-now
                         last-time)
                      1E9)]
    (if (> difference 1)
      (do (.setTitle (:frame window)
                     (str (/ 1
                             (/ difference
                                frames-since-last-update))))
          (assoc window
            :last-fps-time time-now
            :frames-since-last-update 1))
      (assoc window
        :frames-since-last-update (inc frames-since-last-update)))))

(defn initialize-gl []
  (GL11/glClearColor 0 0 0 0)
  (GL11/glEnable GL11/GL_BLEND)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glColorMask true, true, true, true)
  (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA))

(defn request-close []
  #_(awt-input/add-event {:type :close-requested}))

(defn request-redraw []
  #_(awt-input/add-event {:type :redraw-requested}))

(defn create [initial-width initial-height]
  (let [canvas (Canvas.)
        window-atom  (atom (map->Window {:frame (proxy [Frame] []
                                                  (paint [g] (println "frame paint")))
                                         :canvas canvas
                                         :close-requested false
                                         :resize-requested false
                                         :width 0
                                         :height 0}))]

    (swap! window-atom initialize-fps)

    (.addComponentListener canvas
                           (proxy [ComponentAdapter] []
                             (componentResized [e]
                               #_(awt-input/add-event {:type :resize-requested
                                                     :width (-> canvas .getSize .getWidth)
                                                     :height (-> canvas .getSize .getHeight)})
                               (swap! window-atom assoc
                                      :width (-> canvas .getSize .getWidth)
                                      :height (-> canvas .getSize .getHeight)
                                      :resize-requested true))))



    (doto (:frame @window-atom)
      (.add canvas)
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowClosing [e]
           (request-close))

         (windowActivated [e]
           (request-redraw))))
      (.setSize initial-width initial-height)
      .show)
    (.setFocusable canvas false)

    (awt-input/set-key-listener (:frame @window-atom))

    ;;(.requestFocus canvas)

    (Display/setParent canvas)
    (Display/create)
    (initialize-gl)

    ;;    (update 10 true)

    window-atom))

(defn close [window]
  (println "Destroying window")
  (Display/destroy)
  (.dispose (:frame window))
  (assoc window :close-requested false))

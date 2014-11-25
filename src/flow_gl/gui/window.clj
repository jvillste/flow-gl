(ns flow-gl.gui.window)

(defprotocol Window
  (set-renderer [this renderer])
  (event-channel [this])
  (visible? [this])
  (width [this])
  (height [this])
  (close [this]))

(defmacro render [window gl & body]
  `(set-renderer ~window (fn [~gl] ~@body)))

(defmacro render-constantly [window gl & body]
  `(let [value-atom# (atom {})]
     (set-renderer ~window
                   (fn [~gl]
                     (reset! value-atom#
                             (do ~@body))))
     @value-atom#))

(defmacro with-gl [window gl & body]
  `(let [value-atom# (atom {})]
     (set-renderer ~window
                   (fn [~gl]
                     (reset! value-atom#
                             (do ~@body))))
     (set-renderer ~window nil)
     @value-atom#))

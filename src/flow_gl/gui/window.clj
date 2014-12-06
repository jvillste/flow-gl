(ns flow-gl.gui.window)

(defprotocol Window
  (run-with-gl [this runner])
  (event-channel [this])
  (swap-buffers [this])
  (visible? [this])
  (width [this])
  (height [this])
  (close [this]))

(defmacro with-gl [window gl & body]
  `(let [value-atom# (atom {})]
     (run-with-gl ~window
                   (fn [~gl]
                     (reset! value-atom#
                             (do ~@body))))
     @value-atom#))

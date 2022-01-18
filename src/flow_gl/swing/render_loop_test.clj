(ns flow-gl.swing.render-loop-test
  (:import [java.awt Canvas Color Font]
           java.awt.geom.Rectangle2D$Double
           javax.swing.JFrame
           java.awt.event.MouseMotionAdapter))

(defn start []
  (let [last-paint-time-atom (atom (System/currentTimeMillis))
        frame-rates-atom (atom [])
        mouse-event-atom (atom nil)
        j-frame (JFrame.)
        canvas (Canvas.)]

    (.add (.getContentPane j-frame)
          canvas)

    (doto j-frame
      (.setSize 200 100)
      (.setVisible true))

    (.setIgnoreRepaint canvas true)
    (.createBufferStrategy canvas 2)
    (.addMouseMotionListener canvas
                             (proxy [MouseMotionAdapter] []
                               (mouseMoved [event]
                                 (reset! mouse-event-atom {:x (.getX event)}))))

    (let [buffer-strategy (.getBufferStrategy canvas)
          font (Font. "Dialog" Font/PLAIN 30)
          frame-rate-sample-count 100]
      (while true
        (let [time-now (System/currentTimeMillis)
              graphics (.getDrawGraphics buffer-strategy)]
          (try
            (swap! frame-rates-atom (fn [frame-rates]
                                      (take frame-rate-sample-count
                                            (conj frame-rates
                                                  (/ 1000
                                                     (max 1
                                                          (- time-now
                                                             @last-paint-time-atom)))))))

            (reset! last-paint-time-atom time-now)

            (doto graphics
              (.setColor (Color. (float 1)
                                 (float 1)
                                 (float 1)
                                 (float 1)))
              (.fill (Rectangle2D$Double. (double 0)
                                          (double 0)
                                          (double 200)
                                          (double 100)))
              (.setColor (Color. (float 0)
                                 (float 0)
                                 (float 0)
                                 (float 1)))
              (.setFont font)
              (.drawString (str (int (/ (reduce + @frame-rates-atom)
                                        frame-rate-sample-count))
                                " "
                                (:x @mouse-event-atom))
                           0
                           50))

            #_(Thread/sleep 20)

            (finally
              (.dispose graphics)
              (.show buffer-strategy))))))))


(comment
  (start)
  )

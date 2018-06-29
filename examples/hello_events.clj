(ns examples.hello-events
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture])
            (flow-gl.graphics [text :as text]
                              [font :as font])
            (flow-gl.gui [events :as events]
                         [event-queue :as event-queue])))


(defn start []
  (let [width 300
        height 300
        event-queue (event-queue/create)
        window (window/create width
                              height
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (loop [counter 0]
      (window/render window gl

                     (opengl/clear gl 0 0 0 1)

                     (-> (text/create-buffered-image [0 0 1 1]
                                                     (font/create "LiberationSans-Regular.ttf" 20)
                                                     (str "Hello World " counter))
                         (texture/create-for-buffered-image gl)
                         (textured-quad/create gl)
                         (textured-quad/render gl)
                         (textured-quad/delete gl)))

      (let [event (event-queue/dequeue-event-or-wait event-queue)]
        (cond (events/key-pressed? event :enter)
              (recur (inc counter))

              (events/key-pressed? event :esc)
              (window/close window)

              (= (:type event)
                 :close-requested)
              (window/close window)

              :default (recur counter))))))


;(start)


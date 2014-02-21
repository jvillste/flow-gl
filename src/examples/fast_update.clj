(ns examples.fast-update
  (:require [flow-gl.utils :as utils]

            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events])

            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]))
  (:use flow-gl.utils))

(defn start-view [view event-handler initial-state]
  (let [event-queue (event-queue/create)
        window (window/create 300
                              300
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (loop [state initial-state]
        (let [layoutable (view state)]
          (println "layoutable " (str layoutable))
          (window/render window gl
                         (opengl/clear gl 0 0 0 1)
                         (doseq [command (drawable/drawing-commands (layout/layout layoutable
                                                                                   (window/width window)
                                                                                   (window/height window)))]
                           (doto (command/create-runner command gl)
                             (command/run gl)
                             (command/delete gl)))))

        (let [event (event-queue/dequeue-event-or-wait event-queue)]
          (if (= (:type event)
                 :close-requested)
            (window/close window)

            (recur (event-handler state event)))))

      (catch Exception e
        (window/close window)
        (throw e)))))


(defn text-view [text]
  (drawable/->Text text
                   (font/create "LiberationSans-Regular.ttf" 20)
                   [1 1 1 1]))

(defn task-view [task]
  (layout/->VerticalStack [(text-view (:task task))
                           (layout/->Margin 0 0 0 10
                                            [(layout/->VerticalStack (map task-view (:children task)))])]))

(def data [{:task "1"
            :children [{:task "1.1"}
                       {:task "1.2"}]}
           {:task "2"
            :children [{:task "2.1"}
                       {:task "2.2"}
                       {:task "2.3"}]}
           {:task "3"
            :children [{:task "3.1"}
                       {:task "3.2"}]}])

(defn view [state]
  (println "view for " state)
  (layout/->VerticalStack (map task-view state)))

(defn handle-event [state event]
  (cond
   :default state))

(defn start []
  (start-view view
              handle-event
              data))

;;(start)

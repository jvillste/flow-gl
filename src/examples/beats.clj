(ns examples.beats
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
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl))

(defn start-view [view event-queue event-handler initial-state]
  (let [window (window/create 300
                              400
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (loop [cached-runnables {}]

        (println "rendering")
        (let [live-runnables (try (let [layoutable (time (@view @initial-state))
                                        layout (time (layout/layout layoutable
                                                                    (window/width window)
                                                                    (window/height window)))
                                        drawing-commands (time (doall (drawable/drawing-commands layout)))
                                        cached-runnables-atom (atom cached-runnables)
                                        unused-commands-atom (atom nil)]
                                    ;;                               (clojure.pprint/pprint (read-string (str layout)))
                                    (window/render window gl
                                                   (opengl/clear gl 0 0 0 1)
                                                   (time (doseq [runnable (time (doall (reduce (fn [runnables command]
                                                                                                 (if (contains? @cached-runnables-atom command)
                                                                                                   (conj runnables (get @cached-runnables-atom command))
                                                                                                   (let [runnable (command/create-runner command gl)]
                                                                                                     (swap! cached-runnables-atom assoc command runnable)
                                                                                                     (conj runnables runnable))))
                                                                                               []
                                                                                               drawing-commands)))]

                                                           ;;(println (type runnable))
                                                           (command/run runnable gl)))

                                                   (let [unused-commands (filter (complement (apply hash-set drawing-commands))
                                                                                 (keys @cached-runnables-atom))]
                                                     (doseq [unused-command unused-commands]
                                                       (println "deleting " (type unused-command))
                                                       (if-let [unused-runnable (get unused-command @cached-runnables-atom)]
                                                         (command/delete unused-runnable gl)))

                                                     (reset! unused-commands-atom unused-commands)))

                                    (apply dissoc @cached-runnables-atom @unused-commands-atom))
                                  (catch Exception e
                                    (println e)))]

          (let [event (event-queue/dequeue-event-or-wait event-queue)]
            (if (= (:type event)
                   :close-requested)
              (window/close window)
              (recur live-runnables)))))

      (catch Exception e
        (window/close window)
        (throw e)))))


(defn handle-event [state event]
  (cond
   :default state))


(def beats (let [beats (->> (cycle [1 2 3 4])
                            (take (* 4 4)))]
             [beats
              (concat (repeat 3 "X" )  (take (* 3 4) (drop 3 beats)))
              (concat (repeat 2 "X" )  (take (* 3 4) (drop 2 beats)))
              (concat (repeat 1 "X" )  (take (* 3 4) (drop 1 beats)))]))

(defn text [text color]
  (drawable/->Text (str text)
                   (font/create "LiberationSans-Regular.ttf" 25)
                   color))

(defn beat-row [name-size-group beats name row-number drop-colors]
  (let [colors (drop drop-colors (cycle (concat (repeat 4 [1 1 1 1])
                                                (repeat 4 [1 1 1 0.8]))))]
    (layout/->HorizontalStack (concat [(layout/->Margin 0 10 0 0
                                                        [(layout/size-group-member name-size-group
                                                                                   :width
                                                                                   (text name [1 1 1 1]))])]
                                      (map (fn [beat color]
                                             (layout/->FixedSize 18 22
                                                                 [(text beat color)]))
                                           (nth beats row-number) colors)))))

(defn view [beats]
  (let [name-size-group (layout/create-size-group)]
    (layout/->VerticalStack [(beat-row name-size-group beats "Jukka" 0 0)
                             (beat-row name-size-group beats "Risto" 1 1)
                             (beat-row name-size-group beats "Arttu" 2 2)
                             (beat-row name-size-group beats "Foobar" 3 3)])))

(defonce event-queue (atom (event-queue/create)))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (start-view #'view
                                      @event-queue
                                      #'handle-event
                                      #'beats)))))

(event-queue/add-event @event-queue {})



;;(start)

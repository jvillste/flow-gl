(ns examples.research
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
                                 [window :as window])
            ;;clojure.core.logic
            )
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
                   "close-requested")
              (window/close window)
              (recur live-runnables)))))

      (catch Exception e
        (window/close window)
        (throw e)))))

(defn author [id name]
  {:type :author
   :id id
   :name name})

(defn system [id name]
  {:type :system
   :id id
   :name name})

(defn article [id name]
  {:type :article
   :id id
   :name name})

(def data [(author :spj "Simon Peyton Jones")
           (system :haggis "Haggis")
           ])


(defn handle-event [state event]
  (cond
   :default state))


(defn text [text color]
  (drawable/->Text (str text)
                   (font/create "LiberationSans-Regular.ttf" 25)
                   color))



(defn view [beats]
  (layout/->VerticalStack []))

(defonce event-queue (atom (event-queue/create)))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (start-view #'view
                                      @event-queue
                                      #'handle-event
                                      #'data)))))

(event-queue/add-event @event-queue {})



;;(Start)

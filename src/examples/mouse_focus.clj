(ns examples.mouse-focus
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


(defn apply-mouse-event-handlers [state root-layout child-paths event]
  (reduce (fn [state child-path]
            ((get-in root-layout (conj child-path :handle-mouse-event))  state event))
          state
          (filter (fn [child-path]
                    (:handle-mouse-event (get-in root-layout child-path)))
                  child-paths)))

(defn start-view [view event-queue event-handler initial-state]
  (let [window (window/create 300
                              400
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (println "initial " initial-state)
    (try
      (loop [state initial-state
             previous-state {}
             cached-runnables {}]

        (println "rendering")
        (let [layoutable (view state)
              layout (layout/layout layoutable
                                    (window/width window)
                                    (window/height window))
              drawing-commands (doall (drawable/drawing-commands layout))
              cached-runnables-atom (atom cached-runnables)
              unused-commands-atom (atom nil)]

          (when (not= state previous-state)
            (window/render window gl
                           (opengl/clear gl 0 0 0 1)
                           (doseq [runnable (doall (reduce (fn [runnables command]
                                                             (if (contains? @cached-runnables-atom command)
                                                               (conj runnables (get @cached-runnables-atom command))
                                                               (let [runnable (command/create-runner command gl)]
                                                                 (swap! cached-runnables-atom assoc command runnable)
                                                                 (conj runnables runnable))))
                                                           []
                                                           drawing-commands))]

                             ;;(println (type runnable))
                             (command/run runnable gl))

                           (let [unused-commands (filter (complement (apply hash-set drawing-commands))
                                                         (keys @cached-runnables-atom))]
                             (doseq [unused-command unused-commands]
                               (if-let [unused-runnable (get unused-command @cached-runnables-atom)]
                                 (command/delete unused-runnable gl)))

                             (reset! unused-commands-atom unused-commands))))

          (let [event (event-queue/dequeue-event-or-wait event-queue)
                live-commands (apply dissoc @cached-runnables-atom @unused-commands-atom)]
            (cond
             (= (:source event)
                :mouse)
             (let [child-paths-under-mouse (layout/children-in-coordinates-list layout [] (:x event) (:y event))]

               (doseq [child-path child-paths-under-mouse]
                 (println "type under mouse " (type (get-in layout child-path))))

               (recur (apply-mouse-event-handlers state
                                                  layout
                                                  child-paths-under-mouse
                                                  event)
                      state
                      live-commands))

             (= (:type event)
                :close-requested)
             (do (println "closing")
                 (window/close window))

             :default
             (recur (event-handler state event)
                    state
                    live-commands)))))

      (catch Exception e
        (window/close window)
        (throw e)))))


(defn click-counter-view [state state-path]
  (assoc (layout/->Box 10 [(drawable/->Rectangle 0
                                                 0
                                                 [0 0.5 0.5 1])
                           (drawable/->Text (str (:count state))
                                            (font/create "LiberationSans-Regular.ttf" 15)
                                            [0 0 0 1])])
    :handle-mouse-event (fn [state event]
                          (if (= :mouse-clicked (:type event))
                            (update-in state (conj state-path :count) inc)
                            state))))

(defn view [state]
  (layout/->VerticalStack (map-indexed (fn [index counter]
                                         (click-counter-view counter [:counters index]))
                                       (:counters state))))


(defn handle-event [state event]
  (cond
   :default
   state))

(defonce event-queue (atom (event-queue/create)))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (start-view #'view
                                      @event-queue
                                      #'handle-event
                                      {:counters [{:count 0}{:count 0}{:count 0}]}))))

  #_(start-view view
                @event-queue
                handle-event
                (model-to-view-state log initial-view-state)))

(event-queue/add-event @event-queue {})

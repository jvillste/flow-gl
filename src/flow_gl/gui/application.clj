(ns flow-gl.gui.application
  (:require (flow-gl.opengl.jogl [window :as window])
            (flow-gl.gui [view :as view])
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.dataflow :as dataflow]
            [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.opengl :as opengl]
            [flow-gl.gui.event-queue :as event-queue]
            [flow-gl.gui.events :as events])
  (:import [java.io PrintWriter StringWriter]))

(defonce state-atom-atom (atom nil))
(defonce event-queue-atom (atom nil))

(defn wait-for-frame-end-time [frame-start-time framerate]
  (let [frame-length (/ 1E9
                        framerate)
        time-spent-until-now (- (System/nanoTime)
                                frame-start-time)]
    (Thread/sleep (max 0
                       (- frame-length
                          time-spent-until-now)))))

(defn render-loop [window state-queue framerate]
  (try (debug/do-debug :render "starting render loop")

       (loop [frame-start-time (System/nanoTime)]
         (let [state (.take state-queue)]
           (debug/do-debug :render "rendering new state")
           (println "rendring ")
           (if (:closing state)
             (do (let [gl (window/start-rendering window)]
                   (opengl/dispose gl)
                   (window/end-rendering window)
                   (window/close window)))
             (do (let [gl (window/start-rendering window)]
                   (view/update-gpu state gl)
                   (window/end-rendering window))
                 (wait-for-frame-end-time frame-start-time framerate)
                 (recur (System/nanoTime))))))
       (debug/do-debug :render "render loop exit")
       (catch Exception e
         (println "Exception in render loop: " e)
         (let [string-writer (StringWriter.)]
           (.printStackTrace e (PrintWriter. string-writer))
           (println (.toString string-writer)))

         (window/close window)
         (throw e))
       (finally (debug/write-log)))

  (println "render-loop exit")
  (debug/do-debug :render "render-loop exit"))

(defn event-loop [state-atom state-queue event-queue]
  (loop []
    (if (view/is-time-dependant? @state-atom)
      (do (swap! state-atom view/update-time)
          (let [events (event-queue/dequeue-events event-queue)]
            (if (not (empty? events))
              (do (debug/do-debug :events "handling events " events)
                  (view/update state-atom events)))))
      (let [events (event-queue/dequeue-events-or-wait event-queue)]
        (debug/do-debug :events "handling events " events)
        (debug/write-log)
        (view/update state-atom events)))
    (.put state-queue @state-atom)
    (swap! state-atom dataflow/reset-changes)
    (when (not (:closing @state-atom))
      (recur)))

  (println "event loop exit")
  (debug/do-debug :events "event loop exit"))

(defn start-window [root-view & {:keys [handle-event initialize width height framerate]
                                 :or {handle-event (fn [state event] state)
                                      initialize (fn [state state-atom] state)
                                      width 700
                                      height 500
                                      framerate 30}} ]
  (debug/reset-log)
  (let [state-queue (java.util.concurrent.SynchronousQueue.)]
    (try
      (let [state-atom (-> (view/create width height handle-event root-view)
                           ;;(assoc :window-atom window-atom)
                           (atom))
            event-queue (event-queue/create)
            window (window/create width height event-queue)]

        (reset! state-atom-atom state-atom)

        (reset! event-queue-atom event-queue)

        (swap! state-atom
               (fn [state]
                 (-> state
                     (initialize state-atom)
                     (dataflow/propagate-changes)
                     (as-> view-state
                           (do
                             (println "initializing 1 " (:gpu-state view-state))
                             (flow-gl.debug/debug :initialization "Initial view state:")
                             (base-dataflow/debug-dataflow view-state)
                             view-state)))))

        (let [gl (window/start-rendering window)]
          (opengl/initialize gl)
          (view/initialize-gpu-state @state-atom gl)
          (window/end-rendering window))

        (.start (Thread. (fn [] (render-loop window state-queue framerate))))

        (.put state-queue @state-atom)

        (event-loop state-atom state-queue event-queue))
      (catch Exception e
        (println "Exception in event loop: " e)
        (let [string-writer (StringWriter.)]
          (.printStackTrace e (PrintWriter. string-writer))
          (println (.toString string-writer)))

        (.offer state-queue {:closing true})

        (throw e))
      (finally (debug/write-log)))))

(defn close []
  (event-queue/add-event @event-queue-atom
                         (events/create-close-requested-event)))

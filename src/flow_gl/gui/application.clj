(ns flow-gl.gui.application
  (:require (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window])
            (flow-gl.gui [view :as view])
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.dataflow :as dataflow]
            [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.dataflow.triple-dataflow :as triple-dataflow]
            [flow-gl.gui.event-queue :as event-queue])
  (:import [java.io PrintWriter StringWriter]))

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
         (println "waiting for new state to render")
         (let [state (.take state-queue)]
           (debug/do-debug :render "rendering new state")
           (println "rendering in render loop")
           (if (:closing state)
             (window/close window)

             (do (window/render window gl
                                (view/update-gpu state gl))
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
        (println "handling events " events)
        (debug/write-log)
        (view/update state-atom events)))
    (println "sending new state to state queue")
    (.put state-queue @state-atom)
    (swap! state-atom dataflow/reset-changes)
    (when (not (:closing @state-atom))
      (recur)))

  (println "event loop exit")
  (debug/do-debug :events "event loop exit"))

(defn create-window [root-view & {:keys [handle-event root-view-initializer initialize width height framerate]
                                  :or {handle-event (fn [state event] state)
                                       initialize (fn [state state-atom] state)
                                       root-view-initializer identity
                                       width 700
                                       height 500
                                       framerate 30}} ]
  (debug/reset-log)
  (let [state-queue (java.util.concurrent.SynchronousQueue.)]
    (try
      (let [event-queue (event-queue/create)
            state-atom (-> (view/create width height handle-event root-view root-view-initializer)
                           ;;(assoc :window-atom window-atom)
                           (assoc :event-queue event-queue)
                           (atom))

            window (window/create width height opengl/initialize opengl/resize event-queue)]

        (println "calling initialize")
        (swap! state-atom
               (fn [state]
                 (-> state
                     (initialize state-atom)
                     (dataflow/propagate-changes)
                     (as-> view-state
                           (do
                             (println "initial window state " view-state)
                             (flow-gl.debug/debug :initialization "Initial view state:")
                             (base-dataflow/debug-dataflow view-state)
                             view-state)))))

        (window/render window gl
                       (view/initialize-gpu-state @state-atom gl))

        (println "starting render-loop")

        (.start (Thread. (fn [] (render-loop window state-queue framerate))))

        (.put state-queue @state-atom)

        (.start (Thread. (fn [] (event-loop state-atom state-queue event-queue)))))
      (catch Exception e
        (println "Exception in event loop: " e)
        (let [string-writer (StringWriter.)]
          (.printStackTrace e (PrintWriter. string-writer))
          (println (.toString string-writer)))

        (.offer state-queue {:closing true})

        (throw e))
      (finally (debug/write-log)))))

(defn event-queue-from-view-state [view-state]
  (-> view-state
      ::triple-dataflow/dataflow
      :event-queue))

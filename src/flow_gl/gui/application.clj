(ns flow-gl.gui.application
  (:require (flow-gl.opengl.jogl [window :as window])
            (flow-gl.gui [view :as view])
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.dataflow :as dataflow]
            [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.opengl :as opengl]
            [flow-gl.gui.event-queue :as event-queue])
  (:import [java.io PrintWriter StringWriter]))

(defonce state-atom-atom (atom nil))

(defn wait-for-frame-end-time [frame-start-time framerate]
  (let [frame-length (/ 1E9
                        framerate)
        time-spent-until-now (- (System/nanoTime)
                                frame-start-time)]
    (Thread/sleep (- frame-length
                     time-spent-until-now))))

(defn render-loop [window initial-state state-queue framerate]
  (try (debug/do-debug :render "starting render loop")

       (loop [frame-start-time (System/nanoTime)]
         (let [state (.take state-queue)]
           (debug/do-debug :render "rendering new state")
           (if (:closing state)
             (do (opengl/dispose)
                 (window/close window))
             (do (view/update-gpu state)
                 (window/update window)
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

(defn event-loop [state-atom event-queue state-queue]
  (loop []
    (println "state type " (type @state-atom))
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

  (try
    (let [state-queue (java.util.concurrent.SynchronousQueue.)
          state-atom (-> (view/create width height handle-event root-view)
                         ;;(assoc :window-atom window-atom)
                         (atom))
          event-queue (event-queue/create)
          window (window/create event-queue width height)]

      (reset! state-atom-atom state-atom)

      (swap! state-atom
             (fn [state]
               (-> state
                   (initialize state-atom)
                   (dataflow/propagate-changes)
                   (as-> view-state
                         (flow-gl.debug/debug :initialization "Initial view state:")
                         (base-dataflow/debug-dataflow view-state)
                         view-state))))

      (let [gl (window/get-gl window)])
      (opengl/initialize)
      (view/initialize-gpu-state initial-state)

      (.start (Thread. (fn [] (render-loop window @state-atom state-queue framerate))))

      (event-loop state-atom state-queue))
    (catch Exception e
      (println "Exception in event loop: " e)
      (let [string-writer (StringWriter.)]
        (.printStackTrace e (PrintWriter. string-writer))
        (println (.toString string-writer)))

      (.offer state-queue {:closing true})

      (throw e))
    (finally (debug/write-log))))

(ns flow-gl.gui.application
  (:require (flow-gl.opengl [window :as window])
            (flow-gl.gui [view :as view])
            [flow-gl.debug :as debug]
            [flow-gl.dataflow :as dataflow]
            [flow-gl.opengl :as opengl]
            [flow-gl.gui.awt-input :as awt-input]))

(defonce state-atom-atom (atom nil))

(defn render-loop [width height state-queue framerate]
  (let [window-atom (window/create width height)]
    (try (debug/do-debug :render "starting render loop")
         (opengl/initialize)
         (loop []
           (let [state (.take state-queue)]
             (debug/do-debug :render "rendering new state")
             (if (:closing state)
               (do (opengl/dispose)
                   (swap! window-atom window/close))
               (do (view/update-gpu state)
                   (window/update @window-atom framerate)
                   ;;(swap! window-atom window/show-fps)
                   (recur)))))
         (debug/do-debug :render "render loop exit")
         (catch Exception e
           (println "Exception in render loop: " e)
           (.printStackTrace e)
           (swap! window-atom window/close)
           (throw e))
         (finally (debug/write-log)))))

(defn event-loop [state-atom state-queue]
  (try
    (debug/do-debug :events "starting event loop")
    (loop []
      (if (view/is-time-dependant? @state-atom)
        (do (swap! state-atom view/update-time)
            (let [events (awt-input/dequeue-events)]
              (if (not (empty? events))
                (do (debug/do-debug :events "handling events " events)
                    (view/update state-atom events)))))
        (let [events (awt-input/dequeue-events-or-wait)]
          (debug/do-debug :events "handling events " events)
          (debug/write-log)
          (view/update state-atom events)))
      (println "sending new state")
      (.put state-queue @state-atom)
      (swap! state-atom dataflow/reset-changes)
      (when (not (:closing @state-atom))
        (recur)))
    (debug/do-debug :events "event loop exit")
    (catch Exception e
      (println "Exception in event loop: " e)
      (.printStackTrace e)
      (.put state-queue (assoc @state-atom :closing true))
      (throw e))
    (finally (debug/write-log))))


(defn start  [root-layoutable-constructor & {:keys [handle-event initialize width height framerate]
                                             :or {handle-event (fn [state view event] state)
                                                  initialize (fn [state state-atom] state)
                                                  width 700
                                                  height 500
                                                  framerate 30}} ]
  (debug/reset-log)
  (let [state-queue (java.util.concurrent.SynchronousQueue.)]

    (.start (Thread. (fn [] (render-loop width height state-queue framerate))))
    (let [state-atom (-> (view/create width height handle-event root-layoutable-constructor)
                         ;;(assoc :window-atom window-atom)
                         (atom))]

      (reset! state-atom-atom state-atom)

      (swap! state-atom
             (fn [state]
               (-> state
                   ;; (assoc :state-atom state-atom)
                   (initialize state-atom)
                   (dataflow/propagate-changes)
                   ((fn [view-state]
                      (flow-gl.debug/debug :initialization "Initial view state:")
                      (flow-gl.debug/debug-all :initialization (dataflow/dependency-tree view-state))
                      view-state)))))

      (event-loop state-atom state-queue))

    nil))

(defn close [application-state]
  (swap! (:window-atom application-state) window/request-close)
  application-state)


(defn start-viewless [update & {:keys [initialize close resize width height framerate]
                                :or {initialize (fn [state state-atom] state)
                                     close (fn [state-atom])
                                     resize (fn [state-atom width height])
                                     width 700
                                     height 500
                                     framerate 30}}]
  (debug/reset-log)
  (let [window-atom (window/create width height)]
    (opengl/initialize)
    (try
      (let [state-atom (-> {}
                           (assoc :window-atom window-atom)
                           (atom))]

        (reset! state-atom-atom state-atom)

        (swap! state-atom initialize state-atom)

        (loop []
          (let [{:keys [resize-requested close-requested width height]} @window-atom]
            (if close-requested
              (do (close state-atom)
                  (opengl/dispose)
                  (swap! window-atom window/close)
                  (reset! state-atom-atom nil))
              (do (window/update framerate true)
                  (swap! window-atom window/show-fps)
                  (when resize-requested
                    (swap! window-atom assoc
                           :resize-requested false)
                    (window/resize width height)
                    (resize state-atom width height))
                  (update state-atom)
                  (recur))))))
      (catch Exception e
        (println "Exception in main loop: " e)
        (.printStackTrace e)
        (swap! window-atom window/close)
        (throw e))
      (finally (debug/write-log)))
    nil))
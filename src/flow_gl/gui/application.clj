(ns flow-gl.gui.application
  (:require (flow-gl.opengl [window :as window])
            (flow-gl.gui [view :as view])
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.dataflow :as dataflow]
            [flow-gl.dataflow.base-dataflow :as base-dataflow]
            [flow-gl.opengl :as opengl]
            [flow-gl.gui.awt-input :as awt-input])
  (:import [java.io PrintWriter StringWriter]))

(defonce state-atom-atom (atom nil))

(defn render-loop [width height initial-state state-queue framerate]
  (let [window-atom (window/create width height)]
    (try (debug/do-debug :render "starting render loop")
         (opengl/initialize)
         (view/initialize-gpu-state initial-state)
         (loop []
           (let [state (.take state-queue)]
             (debug/do-debug :render "rendering new state")
             (if (:closing state)
               (do (opengl/dispose)
                   (swap! window-atom window/close))
               (do (view/update-gpu state)
                   (window/update @window-atom framerate)
                                        ; (swap! window-atom window/show-fps)
                   (recur)))))
         (debug/do-debug :render "render loop exit")
         (catch Exception e
           (println "Exception in render loop: " e)
           (let [string-writer (StringWriter.)]
             (.printStackTrace e (PrintWriter. string-writer))
             (println (.toString string-writer)))

           (swap! window-atom window/close)
           (throw e))
         (finally (debug/write-log)))))

(defn event-loop [state-atom state-queue]
  (loop []
    (println "state type " (type @state-atom))
    (if (view/is-time-dependant? @state-atom)
      (do (swap! state-atom view/update-time)
          (let [events (awt-input/dequeue-events)]
            (if (not (empty? events))
              (do (debug/do-debug :events "handling events " events)
                  (view/update state-atom events)))))
      (let [events (awt-input/dequeue-events-or-wait)]
        (debug/do-debug :events "handling events " events)
        (println "handling in loop " events " type " (type @state-atom))

        (debug/write-log)
        (view/update state-atom events)))
    (.put state-queue @state-atom)
    (swap! state-atom dataflow/reset-changes)
    (when (not (:closing @state-atom))
      (recur)))
  (debug/do-debug :events "event loop exit"))


(defn start  [root-view & {:keys [handle-event initialize width height framerate]
                           :or {handle-event (fn [state event] state)
                                initialize (fn [state state-atom] state)
                                width 700
                                height 500
                                framerate 30}} ]

  (let [state-queue (java.util.concurrent.SynchronousQueue.)]

    (try
      (let [state-atom (-> (view/create width height handle-event root-view)
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
                        (base-dataflow/debug-dataflow view-state)
                        view-state)))))

        (.start (Thread. (fn [] (render-loop width height @state-atom state-queue framerate))))

        (event-loop state-atom state-queue))
      (catch Exception e
        (println "Exception in event loop: " e)
        (let [string-writer (StringWriter.)]
          (.printStackTrace e (PrintWriter. string-writer))
          (println (.toString string-writer)))

        (.offer state-queue {:closing true})

        (throw e))
      (finally (debug/write-log)))))

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


(defn request-close []
  (awt-input/add-event {:type :close-requested}))

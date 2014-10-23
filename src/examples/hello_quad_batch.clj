(ns examples.hello-quad-batch
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            [flow-gl.debug :as debug]
            [flow-gl.csp :as csp]
            [flow-gl.tools.debug-monitor :as debug-monitor]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [quad-view :as quad-view])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch]))
  (:use flow-gl.utils
        clojure.test))

(defn create-texture [drawable]
  (let [buffered-image (buffered-image/create (max 1 (:width drawable))
                                              (max 1 (:height drawable)))]
    (drawable/draw drawable
                   (buffered-image/get-graphics buffered-image))
    buffered-image))

(defn render-quads [window gpu-state-atom quads]

  (debug/set-metric :render-time (System/currentTimeMillis))

  (window/set-display window gl
                      (let [size (opengl/size gl)]
                        (opengl/clear gl 0 0 0 1)
                        
                        (reset! gpu-state-atom
                                (quad-batch/draw-quads @gpu-state-atom
                                                       gl
                                                       quads
                                                       (:width size)
                                                       (:height size))))))

(def ^:dynamic last-event-channel-atom (atom nil))

(defn initialize [quad-batch gl]
  (quad-batch/add-textures quad-batch gl [(buffered-image/create-from-file "pumpkin.png")]))

(defn quads [frame-time]
  (for [i (range 1 10)]
    (let [round-time (* i 3000)]
      {:x (* (/ (mod frame-time round-time)
                round-time)
             100)
       :y (* i 30)
       :texture-id 0})))

(defn wait-for-next-frame [frame-started]
  (let [target-frames-per-second 60]
    (Thread/sleep (max 0
                       (- (/ 1000 target-frames-per-second)
                          (- (System/currentTimeMillis)
                             frame-started))))))

(defn start-view []
  (let [event-channel (async/chan 50)
        window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :reshape opengl/resize
                              :event-channel event-channel)]

    (reset! last-event-channel-atom event-channel)

    (try
      (let [gpu-state-atom (atom (window/with-gl window gl (-> (quad-batch/create gl)
                                                               (initialize gl))))
            quads-to-render (quads 4000)]
        (loop []
          (let [frame-started (System/currentTimeMillis)]
            (render-quads window gpu-state-atom #_quads-to-render (quads frame-started))

            (let [continue (reduce (fn [continue event]
                                     (if (= (:type event)
                                            :close-requested)
                                       false
                                       continue))
                                   true
                                   (csp/drain event-channel 0))]
              (if continue
                (do (wait-for-next-frame frame-started)
                    (recur))
                (window/close window))))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  (start-view)
  #_(debug-monitor/with-debug-monitor
      (.start (Thread. (fn []
                         (start-view)))))
  #_(.start (Thread. (fn []
                     (start-view)))))

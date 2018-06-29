(ns examples.hello-quad-view
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            [flow-gl.debug :as debug]
            #_[flow-gl.csp :as csp]
            #_[flow-gl.tools.debug-monitor :as debug-monitor]
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


(defn render-layout [window gpu-state-atom layout]
  #_(let [removed-texels (get-in @gpu-state-atom [:quad-batch :removed-texels])
          allocated-texels (get-in @gpu-state-atom [:quad-batch :allocated-texels])]
      (debug/set-metric :texel-fill-ratio (/ removed-texels allocated-texels) :ratio? true)
      (debug/set-metric :removed-texels removed-texels)
      (debug/set-metric :allocated-texels allocated-texels))

  #_(debug/set-metric :render-time (System/currentTimeMillis))

  (window/set-display window gl
                      (let [size (opengl/size gl)]
                        (opengl/clear gl 0 0 0 1)
                        (reset! gpu-state-atom
                                (quad-view/draw-layout @gpu-state-atom
                                                       layout
                                                       (:width size)
                                                       (:height size)
                                                       gl)))))

(def ^:dynamic last-event-channel-atom (atom nil))

(defn layout [frame-time]
  {:x 0
   :y 0
   :children (for [i (range 1 2)]
               (let [round-time (* i 1000)
                     phase (/ (mod frame-time round-time)
                              round-time)]
                 (assoc (drawable/->Rectangle 600 #_(* phase
                                                 600)
                                              700
                                              [1 #_(float phase) 1 0 1])
                   :x (* phase
                         100)
                   :y (* i 40)
                   :z i)))})

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
      (let [gpu-state-atom (atom (window/with-gl window gl (quad-view/create gl)))
            layout-to-be-rendered (layout 1000)]

        (loop []
          (let [frame-started (System/currentTimeMillis)]
            (let [layout (layout frame-started)]
              (render-layout window gpu-state-atom layout #_layout-to-be-rendered)

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
                  (window/close window)))))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  #_(start-view)
  #_(debug-monitor/with-debug-monitor
      (.start (Thread. (fn []
                         (start-view)))))
  (.start (Thread. (fn []
                     (start-view)))))

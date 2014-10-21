(ns examples.fractal
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
  (:import [java.io File]
           [java.util Random])
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
                        (opengl/clear gl 1 1 1 1)

                        (reset! gpu-state-atom
                                (quad-batch/draw-quads @gpu-state-atom
                                                       gl
                                                       quads
                                                       (:width size)
                                                       (:height size))))))

(defn files-in-directory [directory-path]
  (.listFiles (File. directory-path)))

(defn photos [archive-path]
  #_[#_(str archive-path "pumpkin.png")
     (str archive-path "3.png")]
  (->> (files-in-directory archive-path)
       (map #(.getPath %))
       (filter (fn [name] (.endsWith name ".png")))))

(def ^:dynamic last-event-channel-atom (atom nil))
(def archive-path "/Users/jukka/Documents/synttäri/")

(defn initialize [quad-batch gl]
  (quad-batch/add-textures quad-batch gl
                           (map buffered-image/create-from-file
                                (photos archive-path))))

(defn circle-coordinates [radius rotation count]

  (let [segment (fn [cos1 sin1 cos2 sin2]
                  [(* radius cos1)
                   (* radius sin1)])
        cos-sin (for [i (range count)]
                  (let [angle (* (+ (* 2 Math/PI rotation)
                                    (/ i count))
                                 2 Math/PI)]
                    [(Math/cos angle)
                     (Math/sin angle)]))]

    (partition 2 (apply concat

                        (let [[cos2 sin2] (first cos-sin)
                              [cos1 sin1] (last cos-sin)]
                          (segment cos1 sin1 cos2 sin2))

                        (for [[[cos1 sin1] [cos2 sin2]] (partition 2 1 cos-sin)]
                          (segment cos1 sin1 cos2 sin2))))))

(defn quad [texture-id size [x y]]
  {:x (- x (/ size 2))
   :y (- y (/ size 2))
   :texture-id texture-id
   :width size
   :height size})



(defn circle-quads [radius size rotation count]
  (map (fn [coordinates]
         (quad 0 size coordinates))
       (circle-coordinates radius rotation count)))

(defn transpose [quad x y]
  (-> quad
      (update-in [:x] + x)
      (update-in [:y] + y)))

(def photo-count (count (photos archive-path)))

(defn photo-id [i]
  (.nextInt (Random. i) photo-count))


(defn quads [frame-time]
  #_(for [i (range 0 photo-count)]
      (quad i 200 [100 (+ 100 (* 200 i))]))

  (let [size 350
        center-image-size 100
        hands 10
        circles 10
        biggest-hand-image-size (/ center-image-size 2)
        image-size-difference (/ biggest-hand-image-size
                                 circles)
        loop-length 50000
        phase (* 0.1 (/ (mod frame-time loop-length)
                  loop-length))
        phase-sin (Math/sin (* phase 2 Math/PI))]
    (->> (concat (loop [image-size biggest-hand-image-size
                        radius (/ center-image-size 1.2)
                        i 0
                        quads []]

                   (if (< i circles)
                     (recur (- image-size image-size-difference)
                            (+ radius (* 0.8 image-size))
                            (inc i)
                            (concat quads
                                    (->> (circle-quads radius
                                                       image-size
                                                       (* phase-sin (/ i circles))
                                                       hands)
                                         (map (fn [quad]
                                                (transpose quad size size))))))
                     quads))
                 [(quad 0 center-image-size [size size])
                  (quad 0 center-image-size [size size])])
         (map-indexed (fn [i quad]
                        (assoc quad
                          :texture-id (photo-id i)))))))



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
  #_(start-view)
  #_(debug-monitor/with-debug-monitor
      (.start (Thread. (fn []
                         (start-view)))))
  (.start (Thread. (fn []
                     (start-view)))))

(ns examples.rocket-decades
  (:require [fungl.application :as application]
            [clojure.java.io :as io]
            (fungl [layouts :as layouts]
                   [cache :as cache])
            (flow-gl.gui [visuals :as visuals]
                         [animation :as animation])
            (flow-gl.graphics [buffered-image :as buffered-image])))

(def rocket (buffered-image/create-from-file (.getPath (io/resource "rocket.png"))))

(defn int-ping-pong [max]
  max
  #_(int (* max (animation/ping-pong 1.5
                                   (animation/phase! :animation)))))

(defn ten [first-index scale]
  (layouts/horizontally-2 {:margin (* scale -65)}
                          (for [index (range 1
                                             (int-ping-pong 10))]
                            (layouts/vertically-2 {:centered true}
                                                  (assoc (visuals/image rocket)
                                                         :width (* scale 150)
                                                         :height (* scale 150 0.6))
                                                  (visuals/text (str (+ first-index index))
                                                                {:color [255 255 255 255]
                                                                 :font-size (int (* scale 43))})))))

(defn rockets []
  (animation/swap-state! animation/start-if-not-running :animation)
  (let [scale #_0.15
        (+ 0.0 (* 5 (animation/ping-pong 14
                                           (animation/phase! :animation))))]
    (layouts/flow (for [century (range 1)]
                    (layouts/with-margins  (* scale 40) 0 0 0
                      (layouts/vertically (for [decade (range 1
                                                              (int-ping-pong 10))]
                                            (ten (+ (* decade 10)
                                                    (* century 100))
                                                 scale))))))))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 1000)
  (application/do-layout #_(cache/call! rockets)
                         (rockets)
                         width
                         height))


(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))

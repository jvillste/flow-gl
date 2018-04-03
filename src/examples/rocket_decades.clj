(ns examples.rocket-decades
  (:require [fungl.application :as application]
            [clojure.java.io :as io]
            (fungl [layouts :as layouts]
                   [cache :as cache])
            (flow-gl.gui [visuals :as visuals]
                         [animation :as animation])
            (flow-gl.graphics [buffered-image :as buffered-image])))

(def rocket (buffered-image/create-from-file (.getPath (io/resource "rocket.png"))))

(defn ten [first-index scale]
  (layouts/horizontally-2 {:margin (* scale -65)}
                          (for [index (range 1 11)]
                            (layouts/vertically-2 {:centered true}
                                                  (assoc (visuals/image rocket)
                                                         :width (* scale 150)
                                                         :height (* scale 150 0.6))
                                                  (visuals/text (str (+ first-index index))
                                                                [255 255 255 255]
                                                                (int (* scale 43)))))))

(defn rockets []
  (let [scale 0.75]
    (layouts/flow (for [century (range 10)]
                    (layouts/with-margins  (* scale 40) 0 0 0
                      (layouts/vertically (for [decade (range 10)]
                                            (ten (+ (* decade 10)
                                                    (* century 100))
                                                 scale))))))))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 1000)
  (application/do-layout (cache/call! rockets) 
                         width
                         height))


(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))


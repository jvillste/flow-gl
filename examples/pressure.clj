(ns examples.pressure
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [visuals :as visuals]
                         [animation :as animation])
            (fungl [layout :as layout]
                   [layouts :as layouts])))


(defn create-scene-graph [width height]
  (-> (assoc (visuals/rectangle [255 255 255 255] 0 0)
             :mouse-event-handler (fn [node event]
                                    (prn (:pressure event))
                                    event))
      (application/do-layout width height)))

(defn start []
  (application/start-window #'create-scene-graph :target-frame-rate 5)
  #_(.start (Thread. (fn []
                       (start-window)))))




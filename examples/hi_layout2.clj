(ns examples.hi-layout2
  (:require [fungl.application :as application]
            (fungl [layout :as layout]
                   [layouts :as layouts])
            (flow-gl.gui [visuals :as visuals]
                         [animation :as animation])))
(def clicks (atom 0))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/start-if-not-running :animation)
  (let [x (animation/ping-pong 10 (animation/phase! :animation))]
    (-> (layouts/vertically (layouts/horizontally (assoc (visuals/rectangle [255 0 0 255]
                                                                            80 80 (* x 200) (* x 200))
                                                         :mouse-event-handler (fn [node event]
                                                                                (when (= :mouse-clicked
                                                                                         (:type event))
                                                                                  (swap! clicks inc))
                                                                                event))
                                                  (visuals/rectangle [0 255 255 255]
                                                                     80 80 200 200))
                            (layouts/horizontally (visuals/rectangle [0 0 255 255]
                                                                     80 80 200 200)
                                                  (visuals/rectangle [255 255 0 255]
                                                                     80 80 200 200))
                            (visuals/text (str (float x)))
                            (visuals/text (str "clicks:" @clicks)))
        (application/do-layout width height))))

(comment
  (with-bindings (application/create-event-handling-state)
    (create-scene-graph 100 100)))


(defn start []
  (application/start-window #'create-scene-graph))

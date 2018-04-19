(ns fungl.component.button
  (:require (fungl.component [text :as text])
            (flow-gl.gui [layouts :as layouts]
                         [visuals :as visuals])))

(defn button-mouse-event-handler [handler arguments node event]
  (when (= :mouse-clicked
           (:type event))
    (apply handler arguments))
  event)


(defn button [node handler arguments]
  (-> node
      (assoc :mouse-event-handler [button-mouse-event-handler handler arguments])))

(defn default-button [message handler]
  (-> (layouts/box 10
                   (visuals/rectangle [255 255 0 255] 30 30)
                   (text/text message))
      (assoc :mouse-event-handler [button-mouse-event-handler handler])))

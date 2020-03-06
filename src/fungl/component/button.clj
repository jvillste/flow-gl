(ns fungl.component.button
  (:require [flow-gl.gui.visuals :as visuals]
            [fungl.layouts :as layouts]))

(defn button-mouse-event-handler [handler arguments node event]
  (when (= :mouse-clicked
           (:type event))
    (apply handler arguments))
  event)

(defn button [message handler]
  (-> (layouts/box 10
                   (visuals/rectangle-2 :color [255 255 0 255]
                                        :corner-arc-radius 30)
                   (visuals/text message))
      (assoc :mouse-event-handler [button-mouse-event-handler handler []])))

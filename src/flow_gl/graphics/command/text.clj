(ns flow-gl.graphics.command.text
  (:require (flow-gl.graphics [text :as text]
                              [command :as command])
            (flow-gl.graphics.command [image :as image])))

(defrecord Text [content font color])

(defn create [content font color] (->Text content font color))


(defn create-runner [text gl]
  (-> (text/create-buffered-image (:color text)
                                  (:font text)
                                  (:content text))

      (image/create-image-runner-for-buffered-image gl)))

(extend Text
  command/Command
  {:create-runner create-runner})

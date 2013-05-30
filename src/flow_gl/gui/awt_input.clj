(ns flow-gl.gui.awt-input
  (:require [flow-gl.debug :as debug])
  (:import
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)))

(def event-queue (java.util.concurrent.LinkedBlockingQueue.))

(defn dequeue-events []
  (if (not (.poll event-queue))
    (list (.take event-queue))
    (loop [events (list)]
      (if (.poll event-queue)
        (recur (conj events (.take event-queue)))
        events))))

(defn add-event [event]
  (debug/do-debug :events "add-event " event)  
  (.put event-queue event))

(defn create-keyboard-event [awt-event]
  {:type ({java.awt.event.KeyEvent/KEY_PRESSED :key-pressed
           java.awt.event.KeyEvent/KEY_RELEASED :key-released
           java.awt.event.KeyEvent/KEY_TYPED :key-typed} (.getID awt-event))
   :key-code (.getKeyCode awt-event)
   :character (let [character (.getKeyChar awt-event)]
                (if (= character java.awt.event.KeyEvent/CHAR_UNDEFINED)
                  nil
                  character))
   :time (.getWhen awt-event)
   :source :keyboard})

(defn set-key-listener [component]
  (.addKeyListener component
                   (proxy [KeyAdapter] []
                     (keyPressed [e]
                       (add-event (create-keyboard-event e))))))
(ns flow-gl.gui.awt-input
  (:require [flow-gl.debug :as debug])
  (:import
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)))

(def event-queue (java.util.concurrent.LinkedBlockingQueue.))

(defn dequeue-events []
  (loop [events (list)]
    (if (.peek event-queue)
      (do (debug/do-debug :events "deque event " (.peek event-queue))
          (recur (conj events (.take event-queue))))
      events)))

(defn dequeue-events-or-wait []
  (if (not (.peek event-queue))
    (list (.take event-queue))
    (dequeue-events)))

(defn add-event [event]
  (debug/do-debug :events "add event " event)
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

(defn key-pressed? [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn set-key-listener [component]
  (.setFocusTraversalKeysEnabled component false)
  (.addKeyListener component
                   (proxy [KeyAdapter] []
                     (keyPressed [e]
                       (add-event (create-keyboard-event e))))))

(def esc java.awt.event.KeyEvent/VK_ESCAPE)
(def tab java.awt.event.KeyEvent/VK_TAB)
(def down java.awt.event.KeyEvent/VK_DOWN)
(def up java.awt.event.KeyEvent/VK_UP)
(def right java.awt.event.KeyEvent/VK_RIGHT)
(def left java.awt.event.KeyEvent/VK_LEFT)
(def space java.awt.event.KeyEvent/VK_SPACE)
(def enter java.awt.event.KeyEvent/VK_ENTER)



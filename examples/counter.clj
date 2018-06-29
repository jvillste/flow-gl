(ns examples.counter
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [drawable :as drawable]
                         [layouts :as layouts])

            [flow-gl.gui.layout-dsl :as l]))

(defn button [view-context text-value handler]
  (layouts/->Box 10 [(->  (drawable/->Rectangle 0
                                                0
                                                [130 130 230 255])
                          (gui/on-mouse-clicked-with-view-context view-context
                                                                  (fn [state event]
                                                                    (handler state))))
                     (l/center
                      (controls/text text-value
                                     [0 0 0 255]))]))

(defn counter-view [view-context state]
  (l/vertically
   (l/preferred
    (l/vertically
     (l/center
      (l/margin 10 10 10 10
                (l/horizontally (controls/text (:count state))
                                (when (even? (:count state))
                                  (l/margin 0 0 0 10
                                            (controls/text "Even!"
                                                           [0 255 0 255]))))))
     (gui/call-and-bind view-context
                        state
                        :message
                        :text
                        controls/text-editor
                        :editor)
     
     (button view-context
             "Add one"
             (fn [state]
               (update-in state [:count] inc)))))
   
   (controls/text state)))

(defn counter [view-context]
  {:local-state {:count 1
                 :message ""}
   :view #'counter-view})


(defonce event-channel (atom nil))

#_(trace/untrace-ns 'examples.counter)
#_(trace/trace-ns 'flow-gl.gui.layouts)
#_(trace/untrace-var 'flow-gl.gui.gui/render-drawables-afterwards)
(trace/trace-var 'flow-gl.gui.gui/apply-keyboard-event-handlers-beforehand)

#_(trace/untrace-var 'flow-gl.gui.controls/button)

(defn start []

  (reset! event-channel
          #_(gui/start-control counter)
          (trace/with-trace
            (gui/start-control counter))))


(when @event-channel
  (gui/redraw-app @event-channel))


(println (meta (l/vertically :foo)))

;; tommi laitila, erkki pullinainen, andrei modeiros cycle.js, rx.js, bacon.js




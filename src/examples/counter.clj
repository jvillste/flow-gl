(ns examples.counter
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls])

            [flow-gl.gui.layout-dsl :as l]
            [clj-http.client :as client]))

(defn counter-view [view-context state]
  (l/vertically (l/margin 10 10 10 10
                          (l/horizontally (controls/text (:count state))
                                          (if (even? (:count state))
                                            (l/margin 0 0 0 10 (controls/text "Even!!" [0 255 0 255])))))
                (controls/button view-context
                                 "Increase!"
                                 false
                                 (fn [state]
                                   (update-in state [:count] inc)))))

(defn counter [view-context]
  {:local-state {:count 1}
   :view #'counter-view})


(defonce event-channel (atom nil))

(defn start []

  (reset! event-channel
          
          #_(gui/start-control counter)

          (trace/with-trace
            (trace/trace-ns 'examples.counter)
            (gui/start-control counter))))

(when @event-channel
  (gui/redraw-app @event-channel))


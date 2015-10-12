(ns examples.counter
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [controls :as controls])

            [flow-gl.gui.layout-dsl :as l]
            [clj-http.client :as client]))

(defn root-view [view-context state]
  (l/vertically (l/margin 10 10 10 10
                          (l/horizontally (controls/text (:count state))
                                          (if (even? (:count state))
                                            (l/margin 0 0 0 10 (controls/text "Even!" [0 255 0 255])))))
                (controls/button view-context
                                 "Increase!"
                                 false
                                 (fn [state]
                                   (update-in state [:count] inc)))))

(defn root [view-context]
  {:local-state {:count 1}
   :view #'root-view})


(defonce event-channel (atom nil))

(defn start []
  #_(gui/start-control root)
  
  (reset! event-channel (gui/start-control root))


  #_(async/thread (trace/with-trace
                    #_(trace/trace-var 'flow-gl.gui.gui/apply-to-state)
                    #_(trace/trace-var 'flow-gl.gui.components.autocompleter/autocompleter)
                    #_(trace/untrace-ns 'flow-gl.gui.components.autocompleter)
                    #_(trace/trace-ns 'flow-gl.gui.components.autocompleter)
                    (gui/start-control root))))

(when @event-channel
  (gui/redraw-app @event-channel))


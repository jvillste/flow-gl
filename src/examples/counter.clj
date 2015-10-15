(ns examples.counter
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [drawable :as drawable])

            [flow-gl.gui.layout-dsl :as l]))

(defn counter-view [view-context state]
  (println "counter-view" flow-gl.debug/dynamic-debug-channel @flow-gl.debug/debug-channel)
  (l/vertically (l/margin 10 10 10 10
                          (l/horizontally (controls/text (:count state))
                                          (if (even? (:count state))
                                            (l/margin 0 0 0 10 (controls/text "Even!!" [0 255 0 255])))))
                (l/preferred (controls/button view-context
                                              "Add one"
                                              false
                                              (fn [state]
                                                (update-in state [:count] inc))))))

(defn counter [view-context]
  {:local-state {:count 1}
   :view #'counter-view})


(defonce event-channel (atom nil))

(defn start []

  (reset! event-channel
          
          #_(gui/start-control counter)

          (trace/with-trace
            (println "starting" flow-gl.debug/dynamic-debug-channel @flow-gl.debug/debug-channel)
            (trace/trace-ns 'examples.counter)
            (trace/trace-ns 'flow-gl.gui.layouts)
            (trace/trace-var 'flow-gl.gui.gui/add-layout-afterwards)
            (gui/start-control counter))))

(when @event-channel
  (gui/redraw-app @event-channel))


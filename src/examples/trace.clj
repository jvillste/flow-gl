(ns examples.trace
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [layout-dsl :as l]
                         [layouts :as layouts])))

(defn b [x] (+ 1 (:a x)))

(defn a [x] (+ 1 (b {:a 1})))

#_(do (trace/trace-ns 'examples.trace)
      (trace/with-trace (a 1)))

#_(trace/untrace-ns 'flow-gl.gui.gui)

(defn view [view-context state]
  (layouts/grid (conj (repeat 10 (repeat 10 (controls/text "bar")))
                      (conj (take 9 (repeat (controls/text "bar")))
                            (controls/text (:count state))))))

(.start (Thread. (fn []
                   (do (trace/trace-ns 'flow-gl.gui.layouts)
                       (trace/trace-ns 'flow-gl.gui.gui)
                       (trace/with-trace (gui/start-control (fn [view-context]
                                                              {:local-state {:count 0}
                                                               :handle-keyboard-event (fn [state event]
                                                                                        (gui/apply-to-local-state state view-context update-in [:count] inc))

                                                               :view #'view})))))))
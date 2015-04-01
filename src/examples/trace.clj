(ns examples.trace
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [layout-dsl :as l])))

(defn b [x] (+ 1 (:a x)))

(defn a [x] (+ 1 (b {:a 1})))

#_(do (trace/trace-ns 'examples.trace)
      (trace/with-trace (a 1)))

#_(trace/untrace-ns 'flow-gl.gui.gui)

(.start (Thread. (fn []
                   (do (trace/trace-ns 'flow-gl.gui.gui)
                       (trace/with-trace (gui/start-control (fn [view-context]
                                                              {:view (fn [view-context state]
                                                                       (controls/text "foo"))})))))))

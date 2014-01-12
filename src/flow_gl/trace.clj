(ns flow-gl.trace
  (:require [clojure.tools.trace :as trace]))

(def traced-namespaces ['flow-gl.gui.view
                        'flow-gl.gui.application
                        'flow-gl.opengl.jogl.window])

(defn start-trace []
  (doseq [namespace traced-namespaces]
    (trace/trace-ns namespace)))

(defn end-trace []
  (doseq [namespace traced-namespaces]
    (trace/untrace-ns namespace)))

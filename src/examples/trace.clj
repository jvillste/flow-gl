(ns examples.trace
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [layout-dsl :as l]
                         [layouts :as layouts])))

(defn b [x] (+ 1 (:a x)))

(defn a [x] (+ 1 (b {:a 1})))

(do (trace/trace-ns 'examples.trace)
      (trace/with-trace (a 1)))

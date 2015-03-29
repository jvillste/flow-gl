(ns examples.trace
  (:require [flow-gl.tools.trace :as trace]))

(defn b [x] (+ 1 x))

(defn a [x] (+ 1 (b x)))

(trace/trace-ns 'examples.trace)

(trace/with-trace (a 1))

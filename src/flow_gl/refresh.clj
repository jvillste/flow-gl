(ns flow-gl.refresh
  (:require [clojure.tools.namespace.repl :as repl]))


(defn refresh []
  (repl/set-refresh-dirs "src/flow_gl/dataflow" "src/flow_gl/gui")

                                        ;(disable-reload! flow-gl.dataflow.hierarchical-dataflow)

  (repl/refresh))

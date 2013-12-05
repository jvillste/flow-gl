(ns flow-gl.refresh
  (:require [clojure.tools.namespace.repl :as repl]
            midje.repl))

(defn load-facts []
  (midje.repl/load-facts 'flow-gl.dataflow.triple-dataflow
                         #_'flow-gl.dataflow.base-dataflow))

(defn refresh []
  (repl/set-refresh-dirs "src/flow_gl/dataflow" #_"src/flow_gl/gui")

                                        ;(disable-reload! flow-gl.dataflow.hierarchical-dataflow)

  (repl/refresh :after 'flow-gl.refresh/load-facts))

(ns flow-gl.refresh
  (:require [clojure.tools.namespace.repl :as repl]
            midje.repl))

(defn load-facts []
  (midje.repl/load-facts #_'flow-gl.dataflow.triple-dataflow
                         #_'flow-gl.dataflow.base-dataflow
                         'flow-gl.utils))

(defn refresh []

  (repl/set-refresh-dirs "src"
                         #_"src/flow_gl/dataflow"
                         #_"src/flow_gl/gui")

                                        ;(disable-reload! flow-gl.dataflow.hierarchical-dataflow)

  (repl/refresh :after 'flow-gl.refresh/load-facts))

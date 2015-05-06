(ns flow-gl.refresh
  (:require [clojure.tools.namespace.repl :as repl]
            midje.repl))


(defn refresh []

  (repl/set-refresh-dirs "src/flow_gl/")

  #_(repl/disable-reload! flow-gl.dataflow.hierarchical-dataflow)

  (repl/refresh))

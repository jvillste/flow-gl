(ns flow-gl.dataflow.serialized-dataflow
  (:require [clojure.core.async :as async]
            [flow-gl.dataflow.dataflow :as dataflow]))

(defn create []
  {::definition-channel (async/chan 10)})

(defn dispose [dataflow]
  (async/close! (::definition-channel dataflow)))

(defn start [dataflow-atom]
  (async/go (loop [definition (async/<! (::definition-channel @dataflow-atom))]
              (when definition
                (let [[cell function] definition]
                  (swap! dataflow-atom dataflow/define cell function)
                  (recur (async/<! (::definition-channel @dataflow-atom))))))))

(defn schedule-definition [dataflow cell function]
  (async/go (async/>! (::definition-channel dataflow) [cel function])))

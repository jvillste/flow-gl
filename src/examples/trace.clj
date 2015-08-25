(ns examples.trace
  (:require [clojure.core.async :as async]
            [flow-gl.tools.trace :as trace]
            [flow-gl.debug :as debug]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [layout-dsl :as l]
                         [layouts :as layouts])))

(defn b [x]
  (trace/log "foo" :bar)
  (+ 1 (:a x)
     #_((trace/tfn fuu [x]
                   x)
        3)))

(defn a [x]
  (+ 1 (b {:a 1})))

(defn start []
  #_(trace/trace-ns 'examples.trace)
  (trace/with-trace
    (let [channel @debug/debug-channel]
      (println "channel" channel)
      (trace/log-to-channel channel "go1")
      (async/go (async/<! (async/timeout 120))
                (trace/log-to-channel channel "go")))
    
    (a 1)
    (Thread/sleep 1000)))


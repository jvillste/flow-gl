(ns examples.trace
  (:require #_[clojure.core.async :as async]
            [flow-gl.tools.trace :as trace]
            #_[flow-gl.debug :as debug]
            #_(flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [layout-dsl :as l]
                         [layouts :as layouts])))

(defn b [x]
  (trace/log "b was called with" x)
  (+ 1 (:a x)))

(defn a [x]
  (+ 1 (b {:a 1
           :b [1 2 3 4]})))

(trace/trace-ns 'examples.trace)

(trace/with-trace-logging
  (a 1))

#_(trace/with-trace
  (a 1))



#_(defn start []
    #_(trace/trace-ns 'examples.trace)
    (trace/with-trace
      (let [channel @debug/debug-channel]
        (println "channel" channel)
        (trace/log-to-channel channel "go1")
        (async/go (async/<! (async/timeout 120))
                  (trace/log-to-channel channel "go")))
      
      (a 1)
      (Thread/sleep 1000)))

#_(defn start []

    (trace/with-trace
      (let [channel @debug/debug-channel]
        (trace/log "go1")
        (async/go (async/<! (async/timeout 120))
                  (trace/log "go")))
      
      (a 1)
      (Thread/sleep 1000)))





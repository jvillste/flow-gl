(ns flow-gl.csp
  (:require [clojure.core.async :as async]))

(defn tap-new
  ([mult]
     (tap-new mult (async/chan)))

  ([mult channel]
     (async/tap mult channel)
     channel))

(defn throttle [input-channel interval]
  (let [mult (async/mult input-channel)
        throttled-channel (async/chan)
        unthrottled-channel (tap-new mult)
        unthrottled-channel-2 (tap-new mult)]

    (async/go-loop [value (async/<! unthrottled-channel-2)]
      (when value
        (async/alt! (async/timeout interval) (do (async/>! throttled-channel value)
                                                 (recur (async/<! unthrottled-channel-2)))
                    unthrottled-channel-2 ([value] (recur value)))))
    [throttled-channel unthrottled-channel]))

(defn throttle-at-constant-rate [input-channel interval]
  (let [throttled-channel (async/chan)
        sliding-channel (async/chan (async/sliding-buffer 1))]

    (async/go-loop []
      (if-let [value (async/<! input-channel)]
        (do (async/>! sliding-channel value)
            (recur))
        (async/close! sliding-channel)))

    (async/go-loop []
      (if-let [value (async/<! sliding-channel)]
        (do (async/>! throttled-channel value)
            (async/<! (async/timeout interval))
            (recur))
        (async/close! throttled-channel)))

    throttled-channel))


#_(let [channel (async/chan)
        throttled-channel (throttle-at-constant-rate channel 100)]
    (async/go-loop []
      (if-let [value (async/<! throttled-channel)]
        (do (println "got" value)
            (recur))
        (println "exitting")))

    (async/put! channel 1)
    (async/put! channel 2)
    (async/put! channel 3)
    (Thread/sleep 1000)
    (async/put! channel 4)
    (async/close! channel))

(defn result-as-channel [function]
  (let [channel (async/chan)]
    (async/thread (async/put! channel (function)))
    channel))

(defn drain [channel timeout]
  (loop [values (if (not timeout)
                  [(async/<!! channel)]
                  (async/alt!! (async/timeout (int timeout)) ([_] [])
                               channel ([value] [value])))]
    (if (= values [nil])
      nil
      (async/alt!! (async/timeout 0) ([_] values)
                   channel ([value] (recur (conj values value)))
                   :priority true))))

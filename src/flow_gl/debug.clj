(ns flow-gl.debug
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async]
            [clojure.pprint :as pprint]
            [flow-gl.thread-inheritable :as thread-inheritable])
  (:import [java.io StringWriter]))

;; DEBUG

(defn pprints [m] 
  (let [w (StringWriter.)]
    (pprint/pprint m w)
    (.toString w)))

(defmacro ppreturn [value]
  `(do (println (name (quote ~value)) (pprints ~value))
       ~value))

(defn print-and-return [message value]
  (println message value)
  value)

(defn debug-println [& values]
  (println (apply str values))
  (last values))



(def debug-channel (thread-inheritable/thread-inheritable nil))

(defmacro with-debug-channel
  [channel & body]
  `(thread-inheritable/inheritable-binding [debug-channel ~channel]
                                           ~@body))


(defn start-log-reading-process [log-atom channel]
  (async/go-loop []
                 (let [entry (async/<! channel)]
                   (when entry
                     (do (swap! log-atom conj entry)
                         (recur))))))

(defmacro with-log [log-atom & body]
  `(let [channel# (async/chan)]
     (start-log-reading-process ~log-atom channel#)
     (with-debug-channel channel# ~@body)
     (async/close! channel#)))

(defn add-timed-entry [& values]
  (when @debug-channel
    (async/>!! @debug-channel
               (conj {:time (.getTime (java.util.Date.))
                      :thread (.getId (Thread/currentThread))}
                     (apply hash-map values)))))

#_(println (with-log (add-timed-entry :message "foo")))

(defn debug-timed [& messages]
  (add-timed-entry :type :log
                   :message (apply str (interpose " " messages))))

(defmacro debug-timed-and-return [message value]
  `(do (add-timed-entry :message ~message
                        :block :start)
       (let [value# ~value]
         (add-timed-entry :message ~message
                          :block :end)
         value#)))

(defn set-metric [key value & metadata]
  (apply add-timed-entry :type :metric
         :key key
         :value value
         metadata))

#_(defn write-timed-log []
    (when @@log
      (with-open [writer (io/writer "debug-log.txt")]
        (let [log @@log]
          (loop [log log
                 last-time (:time (first log))]
            (when-let [line (first log)]
              (.write writer (str (- (:time line) last-time)))
              (.write writer " : ")
              (.write writer (str (:message line)))
              (.write writer "\n")
              (recur (rest log)
                     (:time line))))))))

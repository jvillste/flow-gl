(ns flow-gl.debug
  (:require [clojure.core.async :as async]
            [clojure.pprint :as pprint]
            [flow-gl.thread-inheritable :as thread-inheritable])
  (:import (java.io StringWriter)))



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

;; this is used for new threads
(def debug-channel (thread-inheritable/thread-inheritable nil))

;; this is used for core.async/thread -threads that are pooled but inherit dynamic bindings
(def ^:dynamic dynamic-debug-channel nil)

(defmacro with-debug-channel [channel & body]
  `(thread-inheritable/inheritable-binding [debug-channel ~channel]
                                           (binding [dynamic-debug-channel ~channel]
                                             ~@body)))


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


(defn add-timed-entry-to-channel [channel values]
  (async/>!! channel
             (conj {:time (.getTime (java.util.Date.))
                    :thread (.getId (Thread/currentThread))}
                   (apply hash-map values))))

(defn add-timed-entry [& values]
  (if-let [channel (or dynamic-debug-channel
                       @debug-channel)]
    (add-timed-entry-to-channel channel
                                values)))

#_(println (with-log (add-timed-entry :message "foo")))

(defn debug-timed [& messages]
  (add-timed-entry :type :log
                   :message (apply str (interpose " " messages))))

(defonce block-id-atom (atom 0))

(defn next-block-id []
  (swap! block-id-atom inc))



(defmacro debug-timed-and-return [category value]
  `(let [block-id# (next-block-id)]
     (add-timed-entry :category ~category
                      :block-id block-id#
                      :block :start)

     (let [value# ~value]
       (add-timed-entry :block-id block-id#
                        :block :end)
       value#)))

(defmacro defn-timed [function-name arguments & body]
  (let [id (keyword (name function-name))]
    `(defn ~function-name ~arguments
       (debug-timed-and-return ~id
                               (do ~@body)))))

(defn add-event [category]
  (add-timed-entry :category category
                   :type :event)
  #_(debug-timed-and-return category nil))

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


(defn print-channels [place]
  (println place "thread" (.getId (Thread/currentThread))
           "thread-inheritable-debug-channel" (not (nil? @flow-gl.debug/debug-channel))
           "dynamic-debug-channel" (not (nil? flow-gl.debug/dynamic-debug-channel))))

#_(let [channel (async/chan)]
  (async/go-loop [value (async/<! channel)]
    (when value
      (do (println value)
          (recur (async/<! channel)))))
  (with-debug-channel channel
    (async/go (Thread/sleep 1000)
              (print-channels "go block")
              (try (assert false "it was false")
                   (catch Throwable e
                     (.printStackTrace e *out*)))
              (async/go (Thread/sleep 1000)
                        (print-channels "go block in go block")))

    (async/thread
      (Thread/sleep 1200)
      (print-channels "async thread")
      (async/go (Thread/sleep 1000)
                (print-channels "go block in async thread")))
    
    (.start (Thread. (fn []
                       (Thread/sleep 1100)
                       (print-channels "thread")
                       (async/go (Thread/sleep 1000)
                                 (print-channels "go block in thread")))))

    (print-channels "parent")
    
    (println "parent finished")))

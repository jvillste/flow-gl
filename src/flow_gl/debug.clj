(ns flow-gl.debug
  (:require [clojure.java.io :as io]
            [flow-gl.thread-inheritable :as thread-inheritable]))

;; DEBUG

(def log (thread-inheritable/thread-inheritable (atom nil)))
(defonce active-channels (atom #{}))

(defn create-log [] (atom []))

(defmacro with-log
  [log & body]
  `(thread-inheritable/inheritable-binding [log ~log] ~@body))

(defn set-active-channels [& channels]
  (reset! active-channels (into #{} channels)))

(defn reset-log []
  (reset! @log []))

(defn if-channel-active [channel then else]
  (if (or (contains? @active-channels channel)
          (contains? @active-channels :all))
    then
    else))

(defn append-log [channel messages]
  (when @@log
    (swap! @log conj (str (name channel) " : " (apply str (interpose " " messages))))))

(defmacro debug [channel & messages]
  (if-channel-active channel
                     `(let [messages# (list ~@messages)]
                        (append-log ~channel messages#)
                        ;;                        (swap! log conj (str ~channel " " (apply str messages#)))
                        (last messages#))
                     (last messages)))

(defmacro do-debug [channel & messages]
  (if-channel-active channel
                     `(let [messages# (list ~@messages)]
                        (append-log ~channel messages#)
                        ;;(swap! log conj (str ~channel " " (apply str messages#)))
                        (last messages#))
                     nil))

(defmacro debug-all [channel messages]
  (if-channel-active channel
                     `(doseq [message# ~messages]
                        (debug ~channel message#))
                     nil))

(defmacro debug-drop-last [channel & messages]
  (if-channel-active channel
                     `(let [messages# (list ~@messages)]
                        ;;                        (swap! log conj (str ~channel " " (apply str (drop-last messages#))))
                        (append-log ~channel (drop-last messages#))
                        (last messages#))
                     (last messages)))

(defmacro debug-if [channel condition & messages]
  (if-channel-active channel
                     `(let [messages# (list ~@messages)]
                        (when (condition (last messages#))
                          (append-log ~channel messages#)
                          ;;(swap! log conj (str ~channel " " (apply str messages#)))
                          )
                        (last messages#))
                     (last messages)))

(defn add-timed-entry [& values]
  (when @@log
    (swap! @log conj (conj {:time (.getTime (java.util.Date.))
                            :thread (.getId (Thread/currentThread))}
                           (apply hash-map values)))))
(defn debug-timed [& messages]
  (add-timed-entry :message (apply str (interpose " " messages))))


(defmacro debug-timed-and-return [message value]
  `(do (add-timed-entry :message ~message
                        :block :start)
       (let [value# ~value]
         (add-timed-entry :message ~message
                          :block :end)
         value#)))

(defn write-log []
  (when @@log
    (with-open [writer (io/writer "debug-log.txt")]
      (doseq [line @@log]
        (.write writer line)
        (.write writer "\n")))))

(defn write-timed-log []
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


(defn debug-println [& values]
  (println (apply str values))
  (last values))

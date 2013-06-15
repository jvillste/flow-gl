(ns flow-gl.debug
  (:require [clojure.java.io :as io]))

;; DEBUG

(def log (atom []))
(defonce active-channels (atom #{}))

(defn set-active-channels [& channels]
  (reset! active-channels (into #{} channels)))

(defn reset-log []
  (reset! log []))

(defn if-channel-active [channel then else]
  (if (or (contains? @active-channels channel)
          (contains? @active-channels :all))
    then
    else))

(defn append-log [channel messages]
  (swap! log conj (str (name channel) " : " (apply str messages))))

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

(defn write-log []
  (with-open [writer (io/writer "debug-log.txt")]
    (doseq [line @log]
      (.write writer line)
      (.write writer "\n"))))


(defn debug-println [& values]
  (println (apply str values))
  (last values))
(ns fungl.call-stack
  (:require [clojure.string :as string]
            [medley.core :as medley]))

(defn ignored-class-name? [classname]
  (some #(string/includes? classname %)
        ["callers" "dbg" "clojure.lang" "swank" "nrepl" "eval"]))

(defn callers []
  (let [stack-trace (->> (-> (Throwable.) .fillInStackTrace .getStackTrace)
                         (remove (fn [frame]
                                   (ignored-class-name? (.getClassName frame)))))]
    (->> stack-trace
         (medley/distinct-by #(.getClassName %))
         (remove #(= (.getClassName (first stack-trace))
                     (.getClassName %)))
         (map #(str (.getClassName %) ":" (.getLineNumber %))))))

(defn only-function-name [caller]
  (second (re-find #"\$(.*):" caller)))

(defn remove-namespace [caller]
  (second (re-find #"\$(.*)" caller)))

(defn foo []
  (callers))

(defn bar []
  (foo))

(comment
  (bar)
  )

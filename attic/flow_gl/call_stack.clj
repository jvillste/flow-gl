(ns flow-gl.call-stack)

(defn ignored? [classname]
  (let [ignored #{"callers" "dbg" "clojure.lang" "swank" "nrepl" "eval"}]
    (some #(re-find (re-pattern %) classname) ignored)))

(defn callers []
  (let [fns (map #(str (.getClassName %) ":" (.getLineNumber %))
                 (-> (Throwable.) .fillInStackTrace .getStackTrace))]
    (vec (doall (remove ignored? fns)))))


(defn foo []
  (callers))

(defn bar []
  (foo))

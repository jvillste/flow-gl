(ns fungl.log)

(def ^:private print-log-lock (Object.))

(defonce log-atom (atom []))

(defn print-log [log-line]
  (locking print-log-lock
    (apply prn (:values log-line))))

(def print-ns? #{"fungl.view-compiler"
                 "fungl.layout"})

(defmacro write [& values]
  (if (print-ns? (str (ns-name *ns*)))
    `(print-log {:ns ~(str (ns-name *ns*)) :values ~(vec values)})
    `(do)))

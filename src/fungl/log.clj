(ns fungl.log)

(def ^:private print-log-lock (Object.))
(def ^:private last-log-line-time-atom (atom nil))

(defn print-log [log-line]
  (let [time-now (int (/ (System/nanoTime)
                         1000000))]
    (do ;; locking print-log-lock
      (apply prn
             (- time-now
                (or @last-log-line-time-atom
                    time-now))
             (:values log-line)))
    (reset! last-log-line-time-atom time-now)))

(def ^:private print-ns? #{;; "fungl.view-compiler"
                           ;; "fungl.layout"
                           ;; "fungl.hierarchical-identity-cache"
                           ;; "fungl.application"
                           ;; "fungl.node-image-cache"
                           ;; "flow-gl.gui.visuals"
                           ;; "fungl.node-image-cache"
                           })

(defmacro write [& values]
  (if (print-ns? (str (ns-name *ns*)))
    `(print-log {:ns ~(str (ns-name *ns*)) :values ~(vec values)})
    `(do)))

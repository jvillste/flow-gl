(ns examples.javafx
  (:import [javafx.application Application]
           [javafx.stage Stage]
           [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]
           [javafx.scene Group Scene]))

;; from https://github.com/daveray/upshot/blob/develop/src/upshot/core.clj#L69
;; see https://stackoverflow.com/questions/12009685/javafx-from-clojure-repl

(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))

(defn run-later*
  [f]
  (javafx.application.Platform/runLater f))

(defmacro run-later
  [& body]
  `(run-later* (fn [] ~@body)))

(defn run-now*
  [f]
  (let [result (promise)]
    (run-later
      (deliver result (try (f) (catch Throwable e e))))
    @result))

(defmacro run-now
  [& body]
  `(run-now* (fn [] ~@body)))


(defn paint [graphics-context]
  (.setFill graphics-context (Color/RED))
  (.fillRoundRect graphics-context 110, 360, 30, 30, 10, 10))
#_(paint graphics-context)

(defn start []
  (run-now
   (def stage (Stage.))
   (.show stage)))

(comment
  (run-now
   (let [canvas (Canvas. 200 200)
         group (Group.)]

     (def graphics-context (.getGraphicsContext2D canvas))

     (.add (.getChildren group)
           canvas)

     (.setScene stage
                (Scene. group))
     #_(.show stage)))


  ) ;; TODO: remove-me

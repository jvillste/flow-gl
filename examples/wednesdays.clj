(ns examples.wednesdays)

(defn current-date []
  (let [calendar (java.util.GregorianCalendar.)]
    {:year (.get calendar java.util.Calendar/YEAR)
     :month  (+ 1 (.get calendar java.util.Calendar/MONTH))
     :day (.get calendar java.util.Calendar/DAY_OF_MONTH)}))

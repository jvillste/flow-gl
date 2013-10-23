(ns examples.hours
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                                        ;[animation :as animation]
                         [events :as events]
                         [application :as application]

                                        ;[focus :as focus]
                         )
            (flow-gl.graphics [font :as font])
            (flow-gl [dataflow :as dataflow]))

  (:import [java.util GregorianCalendar Calendar] )
  (:use flow-gl.utils
        flow-gl.gui.layout-dsl))

(def log [{:year 2013
           :month 10
           :day 10
           :sessions [{:start-time {:hour 8 :minute 0}
                       :task "koodausta"}
                      {:start-time {:hour 14 :minute 0}
                       :task "kahvi"}
                      {:start-time {:hour 14 :minute 30}
                       :task "koodausta"}
                      {:start-time {:hour 16 :minute 0}
                       :task "kotiin"}]}
          {:year 2013
           :month 10
           :day 11
           :sessions [{:start-time {:hour 8 :minute 0}
                       :task "koodausta"}
                      {:start-time {:hour 14 :minute 0}
                       :task "kahvi"}
                      {:start-time {:hour 15 :minute 0}
                       :task "koodausta"}
                      {:start-time {:hour 16 :minute 0}
                       :task "kotiin"}]}])


;; TIME

(defn current-date []
  (let [calendar (java.util.GregorianCalendar.)]
    {:year (.get calendar java.util.Calendar/YEAR)
     :month  (+ 1 (.get calendar java.util.Calendar/MONTH))
     :day (.get calendar java.util.Calendar/DAY_OF_MONTH)}))

(defn current-time []
  (let [calendar (java.util.GregorianCalendar.)]
    {:hour (.get calendar java.util.Calendar/HOUR)
     :minute  (.get calendar java.util.Calendar/MINUTE)}))

(defn time-to-minutes [time]
  (+ (* (:hour time)
        60)
     (:minute time)))

(defn minutes-to-time [minutes]
  {:hour (int (Math/floor (/ minutes
                             60)))
   :minute (mod minutes 60)})

(defn time-difference-in-minutes [a b]
  (- (time-to-minutes b)
     (time-to-minutes a)))


(defn time-to-str [time]
  (str (:hour time) ":" (:minute time)))

;; MODEL

(defn create-day []
  (assoc (current-date)
    :sessions []))

(defn create-session []
  {:task "work"
   :start-time (current-time)})

(defn calculate-durations [log]
  (for [[a b] (partition 2 1 log)]
    (assoc a
      :duration-in-minutes (time-difference-in-minutes (:start-time a)
                                                       (:start-time b)))))
(defn sum-up-sessions [day-log]
  (for [task-sessions (vals (group-by :task day-log))]
    {:task (:task (first task-sessions))
     :duration-in-minutes (reduce + (map :duration-in-minutes task-sessions))}))

(defn tasks [log]
  (->> log
       (mapcat :sessions)
       (map :task)
       (apply hash-set)))

(defn suggestions [input alternatives]
  (take 5 (filter #(.startsWith % input)
                  alternatives)))


;;  GUI

(defn text [style string]
  (drawable/->Text string
                   (:font style)
                   (:foreground style)))

(defn session-view [style session]
  (text style (str (:task session)
                   " "
                   (-> session
                       :duration-in-minutes
                       minutes-to-time
                       time-to-str))))

(defn session-edit-view [font session]
  (hs (drawable/->Text (:task session)
                       font
                       [0 0 0 1])
      (drawable/->Text (-> session
                           :start-time
                           time-to-str)
                       font
                       [0 0 0 1])))


(defn time-editor-view [time-editor style subject predicate]
  (text style (-> subject
                  predicate
                  time-to-str)))


(defn day-view [style day]
  (layout/->Margin 0 10 0 0
                   (vs (text style (str (:day day)
                                        "."
                                        (:month day)
                                        "."
                                        (:year day)))

                       (hs (layout/->Margin 0 0 20 0
                                            (layout/grid (forall [session (:sessions day)]
                                                                 [(layout/->Margin 0 0 10 0
                                                                                   (text style (:task session)))
                                                                  (time-editor-view {} style session :start-time)])))

                           (apply vs (forall [session (->> (:sessions day)
                                                           calculate-durations
                                                           sum-up-sessions)]
                                             (session-view style session)))))))


(defn view []

  (layout/->Stack [(drawable/->Rectangle (dataflow/get-global-value :width)
                                         (dataflow/get-global-value :height)
                                         [1 1 1 1])
                   (layout/->Margin 10 0 0 0
                                    (let [style {:font (font/create "LiberationSans-Regular.ttf" 15)
                                                 :foreground [0 0 0 1]}]
                                      (apply vs (forall [day (dataflow/get-global-value :log)]
                                                        (day-view style day)))))]))


(defn handle-event [state event]
  (cond (input/key-pressed? event input/esc)
        (do (application/request-close)
            state)

        (input/key-pressed? event input/enter)
        (dataflow/define-to-global state [:log] (conj (get state [:log])
                                                          (-> (create-day)
                                                              (assoc :sessions [(create-session)]))))

        :default state))



(defonce sa (atom nil))

(defn initialize [state state-atom]
  (reset! sa state-atom)
  (dataflow/define-to state [:log] log))

(defn refresh []
  (when @sa
    (swap! @sa view/set-view view)))

(refresh)


(defn start []
  (application/start view
                     :initialize initialize
                     :handle-event handle-event
                     :framerate 60))


(comment
(.start (Thread. start))
  (start)
  )

(ns examples.hours
  (:require [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events])

            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]))
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl))

(defn start-view [view event-handler initial-state]
  (let [event-queue (event-queue/create)
        window (window/create 300
                              300
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (loop [state initial-state]
        (let [layoutable (view state)]
          (window/render window gl
                         (opengl/clear gl 0 0 0 1)
                         (doseq [command (drawable/drawing-commands (layout/layout layoutable
                                                                                   (window/width window)
                                                                                   (window/height window)))]
                           (doto (command/create-runner command gl)
                             (command/run gl)
                             (command/delete gl)))))

        (let [event (event-queue/dequeue-event-or-wait event-queue)]
          (if (= (:type event)
                 :close-requested)
            (window/close window)

            (recur (event-handler state event)))))

      (catch Exception e
        (window/close window)
        (throw e)))))

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


(defn time-to-string [time]
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




(def initial-text-editor-state
  {:text ""
   :has-focus false})

(defn handle-text-editor-event [state event]
  (cond (:character event)
        (update-in state [:text] conj (:character event))

        (events/key-pressed? :back-space)
        (update-in state [:text] (fn [text] (apply str (drop-last text))))

        :default
        state))

(defn text-editor-view [state]
  (drawable/->Text (:text state)
                   (font/create "LiberationSans-Regular.ttf" 15)
                   (if (:has-focus state)
                     [0 0 0 1]
                     [0.3 0.3 0.3 1])))



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
                       time-to-string))))

(defn session-edit-view [font session]
  (horizontally (drawable/->Text (:task session)
                                 font
                                 [0 0 0 1])
                (drawable/->Text (-> session
                                     :start-time
                                     time-to-string)
                                 font
                                 [0 0 0 1])))


(defn time-editor-view [time-editor style subject predicate]
  (text style (-> subject
                  predicate
                  time-to-string)))

(defn day-summary-view [day style]
  (let [sessions (->> (:sessions day)
                      calculate-durations
                      sum-up-sessions)]

    (apply vertically (concat (for-all [session sessions]
                                       (session-view style session))
                              [(drawable/->Text (str "Yhteensä: " (-> (reduce + (map :duration-in-minutes sessions))
                                                                      minutes-to-time
                                                                      time-to-string))
                                                (:font style)
                                                (:foreground style))]))))

(defn day-view [style day]
  (margin 10 0 0 0
          (vertically (text style (str (:day day) "." (:month day) "." (:year day)))
                      (horizontally (margin 0 0 20 0
                                            (layout/grid (for-all [session (:sessions day)]
                                                                  [(magrgin 0 0 0 10
                                                                           (text style (:task session)))
                                                                   (margin 0 0 0 10
                                                                           (time-editor-view {} style session :start-time))])))

                                    (margin 0 0 0 10
                                            (day-summary-view day style))))))

(defn view [state]
  (layout/->Stack [(drawable/->Rectangle 0
                                         0
                                         [1 1 1 1])
                   (margin 10 0 0 0
                           (let [style {:font (font/create "LiberationSans-Regular.ttf" 15)
                                        :foreground [0 0 0 1]}]
                             (apply vertically (for-all [day state]
                                                        (day-view style day)))))]))

(defn handle-event [state event]
  (cond (events/key-pressed? event :enter)
        (conj state (-> (create-day)
                        (assoc :sessions [(create-session)])))

        :default state))


(defn start []
  (start-view view
              handle-event
              log))


;;(start)

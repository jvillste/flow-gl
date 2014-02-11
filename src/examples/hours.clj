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

(defn start-view [view event-queue event-handler initial-state]
  (let [window (window/create 300
                              300
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (loop [state initial-state]
        (let [layoutable (@view state)]
          (window/render window gl
                         (opengl/clear gl 0 0 0 1)
                         (doseq [command (drawable/drawing-commands (layout/layout layoutable
                                                                                   (window/width window)
                                                                                   (window/height window)))]
                           (doto (command/create-runner command gl)
                             (command/run gl)
                             (command/delete gl)))))

        (let [event (event-queue/dequeue-event-or-wait event-queue)]
          (println "event " event)
          (if (= (:type event)
                 :close-requested)
            (window/close window)

            (recur (event-handler state event)))))

      (catch Exception e
        (window/close window)
        (println "exception " e)
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

(defn set-children-focus-state [parent-state]
  (assoc-in parent-state [:child-views] (vec (map-indexed (fn [index child-state]
                                                            (assoc child-state
                                                              :in-focus (= index (:focus parent-state))))
                                                          (:child-views parent-state)))))

(defn move-focus-forward [state]
  (-> (assoc-in state [:focus] (if (= (:focus state)
                                      (dec (count (:child-views state))))
                                 0
                                 (inc (:focus state))))
      (set-children-focus-state)))






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

(defn session-list-view [sessions style]
  (layout/grid (for-all [session sessions]
                        [(margin 0 0 0 10
                                 (text (assoc style :foreground
                                              (if (:in-focus session)
                                                [1 0 0 1]
                                                [1 1 0 1]))
                                       (:task session)))
                         (margin 0 0 0 10
                                 (time-editor-view {} style session :start-time))])))


(def initial-day-view-state
  {:focus 0})

(defn day-view-model-to-day-view-state [model view-state]
  (-> view-state
      (assoc :child-views (:sessions model)
             :day (:day model)
             :year (:year model)
             :month (:month model))
      (set-children-focus-state)))

(defn day-view-state-to-day-view-model [view-state]
  (-> view-state
      (assoc :sessions (:child-views view-state)
             :day (:day view-state)
             :year (:year view-state)
             :month (:month view-state))))

(defn day-view [day style]
  (layout/->Box 10 [(drawable/->FilledRoundedRectangle 0
                                                       0
                                                       10
                                                       [0.5 0.5 1 1])
                    (let [style (update-in style [:foreground]
                                           (fn [[r g b a]]
                                             (if (:in-focus day)
                                               [r g b a]
                                               [r g b (* 0.5 a)])))]
                      (margin 10 0 0 0
                              (vertically (text style
                                                (str (:day day) "." (:month day) "." (:year day)))
                                          (horizontally (margin 0 0 20 0
                                                                (session-list-view (:child-views day) style))

                                                        (margin 0 0 0 10
                                                                (day-summary-view (day-view-state-to-day-view-model day) style))))))]))




(defn handle-day-view-event [day-view event]
  (cond
   (events/key-pressed? event :down)
   (move-focus-forward day-view)

   :default day-view))

(def initial-view-state
  {:focus 0})

(defn model-to-view-state [model view-state]
  (-> view-state
      (assoc :child-views (vec (map (fn [[child-view-model child-view-state]]
                                      (day-view-model-to-day-view-state child-view-model child-view-state))
                                    (partition 2 (interleave model
                                                             (or (:child-views view-state)
                                                                 (repeat (count model) initial-day-view-state)))))))
      (set-children-focus-state)))

(defn view [state]
  (layout/->Stack [(drawable/->Rectangle 0
                                         0
                                         [1 1 1 1])
                   (margin 10 0 0 0
                           (let [style {:font (font/create "LiberationSans-Regular.ttf" 15)
                                        :foreground [0 0 0 1]}]
                             (apply vertically (->> (for-all [day-view-state (:child-views state)]
                                                             (day-view day-view-state style))
                                                    (interpose (drawable/->Empty 10 10))))))]))

(defn handle-event [state event]
  (cond #_(events/key-pressed? event :down)
        #_(move-focus-forward state)

        :default
        (update-in state [:child-views (:focus state)]
                   (fn [child-view-state]
                     (handle-day-view-event child-view-state event)))))

(defonce event-queue (event-queue/create))

(defn start []
  (.start (Thread. (start-view #'view
                               event-queue
                               handle-event
                               (model-to-view-state log initial-view-state)))))

(event-queue/add-event event-queue {})


;;(start)

;;

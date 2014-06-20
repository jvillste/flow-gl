(ns examples.hours
  (:require [flow-gl.utils :as utils]
            [flow-gl.tools.layoutable-inspector :as layoutable-inspector]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view])

            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window])
            gui.diff)
  (:use flow-gl.utils
        flow-gl.gui.layout-dsl))



(defn apply-mouse-event-handlers [root-layout child-paths event]
  (reduce (fn [root-layout child-path]
            (println "applying mouse handler " child-path)
            (update-in root-layout
                       child-path
                       (get-in root-layout (conj child-path :handle-mouse-event))
                       event))
          root-layout
          (filter (fn [child-path]
                    (:handle-mouse-event (get-in root-layout child-path)))
                  child-paths)))

(defn start-view [view event-queue event-handler initial-state]
  (let [window (window/create 300
                              400
                              :profile :gl3
                              :init opengl/initialize
                              :event-queue event-queue)]

    (try
      (loop [state initial-state
             previous-state nil
             previous-layout nil
             quad-view (window/with-gl window gl
                         (quad-view/create gl))]
        (let [layout (if (= previous-state state)
                       previous-layout
                       (let [layoutable (named-time "view" (view state))]
                         (named-time "layout" (assoc (layout/layout layoutable
                                                                    (window/width window)
                                                                    (window/height window))
                                                :x 0 :y 0))))

              quad-view (if (= previous-state state)
                          quad-view
                          (window/with-gl window gl
                            (named-time "draw layout" (quad-view/draw-layout quad-view
                                                                             layout
                                                                             (window/width window)
                                                                             (window/height window)
                                                                             gl))))]
          #_(spit "/Users/jukka/Downloads/versio.txt" (prn-str layout) :append true)
          (layoutable-inspector/inspect-layoutable layout)
          #_(gui.diff/gui-diff previous-layout layout)
          (let [event (event-queue/dequeue-event-or-wait event-queue)]
            (cond
             #_(= (:source event)
                  :mouse)
             #_(do (println "children in coordinates" )
                   (let [child-paths-under-mouse (layout/children-in-coordinates-list layout [] (:x event) (:y event))]
                     (doseq [child-path child-paths-under-mouse]
                       (println "type under mouse " (type (get-in layout child-path))))

                     (recur (apply-mouse-event-handlers layout
                                                        child-paths-under-mouse
                                                        event)
                            state
                            live-commands)))

             (= (:type event)
                :close-requested)
             (do (println "closing")
                 (window/close window))

             :default
             (recur (named-time "event handler" (event-handler state event))
                    state
                    layout
                    quad-view)))))

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
                      {:start-time {:hour 15 :minute 0}
                       :task "hommia"}
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

(def log [{:year 2013
           :month 10
           :day 10
           :sessions [{:start-time {:hour 8 :minute 0}
                       :task "koodausta"}]}])


;; TIME

(defn current-date []
  (let [calendar (java.util.GregorianCalendar.)]
    {:year (.get calendar java.util.Calendar/YEAR)
     :month  (+ 1 (.get calendar java.util.Calendar/MONTH))
     :day (.get calendar java.util.Calendar/DAY_OF_MONTH)}))

(defn current-time []
  (let [calendar (java.util.GregorianCalendar.)]
    {:hour (.get calendar java.util.Calendar/HOUR_OF_DAY)
     :minute  (.get calendar java.util.Calendar/MINUTE)}))

(defn time-to-minutes [time]
  (+ (* (:hour time)
        60)
     (:minute time)))

(defn time-greater-than? [a b]
  (or (> (:hour a)
         (:hour b))
      (and (= (:hour a)
              (:hour b))
           (> (:minute a)
              (:minute b)))))

#_(tabular
 (fact (time-greater-than? ?a ?b) => ?expected)
 ?a                  ?b                  ?expected
 {:hour 1 :minute 1} {:hour 1 :minute 1} false
 {:hour 2 :minute 1} {:hour 1 :minute 1} true
 {:hour 1 :minute 2} {:hour 1 :minute 1} true
 {:hour 1 :minute 2} {:hour 2 :minute 1} false)


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
   :edited-text ""
   :editing? false
   :has-focus false})

(defn append-character [string character]
  (apply str (vec (concat string
                          (str character)))))

#_(fact (append-character "Foo" \a)
      => "Fooa")

(defn handle-text-editor-event [state event]
  (if (:editing? state)
    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :text (:edited-text state))
         (assoc :editing? false))

     (events/key-pressed? event :back-space)
     (update-in state [:edited-text] (fn [text] (apply str (drop-last text))))

     (and (:character event)
          (= (:type event)
             :key-pressed))
     (update-in state [:edited-text] append-character (:character event))


     :default
     state)

    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :edited-text (:text state))
         (assoc :editing? true))


     :default
     state)))

(defn text-editor-view [state]
  (assoc (drawable/->Text (if (:editing? state)
                            (:edited-text state)
                            (:text state))
                          (font/create "LiberationSans-Regular.ttf" 15)
                          (if (:has-focus state)
                            (if (:editing? state)
                              [0 0 1 1]
                              [0 0 0 1])
                            [0.3 0.3 0.3 1]))
    :handle-mouse-event (fn [state event]
                          (println "got mouse event " (:text state))
                          state)))



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
                                 (text-editor-view (:task session)))
                         (margin 0 0 0 10
                                 (horizontally (text-editor-view (:start-time-hour session))
                                               (text style ":")
                                               (text-editor-view (:start-time-minute session))))])))


(def initial-day-view-state
  {:focus 0})

(defn day-view-model-to-day-view-state [model day-view-state]
  (-> (or day-view-state
          initial-day-view-state)
      (assoc :sessions (vec (for [[session session-view] (partition 2 (interleave (:sessions model)
                                                                                  (repeat {:task initial-text-editor-state
                                                                                           :start-time-hour initial-text-editor-state
                                                                                           :start-time-minute initial-text-editor-state})
                                                                                  #_(or (:sessions day-view-state)
                                                                                        (repeat {:task initial-text-editor-state
                                                                                                 :start-time-hour initial-text-editor-state
                                                                                                 :start-time-minute initial-text-editor-state}))))]
                              {:task (assoc (:task session-view)
                                       :text (:task session))
                               :start-time-hour (assoc (:start-time-hour session-view)
                                                  :text (-> session :start-time :hour str))
                               :start-time-minute (assoc (:start-time-minute session-view)
                                                    :text (-> session :start-time :minute str))}))
             :day (:day model)
             :year (:year model)
             :month (:month model))))

(defn day-view-state-to-day-view-model [view-state]
  {:sessions (for [session (:sessions view-state)]
               {:task (-> session :task :text)
                :start-time {:hour (-> session :start-time-hour :text read-string)
                             :minute  (-> session :start-time-minute :text read-string)}})
   :day (:day view-state)
   :year (:year view-state)
   :month (:month view-state)})

(defn day-view [day style]
  (layout/->Box 10 [(drawable/->FilledRoundedRectangle 0
                                                       0
                                                       10
                                                       [0.5 0.5 1 1])
                    (margin 10 0 0 0
                            (vertically (text style
                                              (str (:day day) "." (:month day) "." (:year day)))
                                        (horizontally (margin 0 0 20 0
                                                              (session-list-view (:sessions day) style))

                                                      (margin 0 0 0 10
                                                              (day-summary-view (day-view-state-to-day-view-model day) style)))))]))


(def initial-view-state
  {:day-in-focus 0
   :session-in-focus 0
   :session-column-in-focus 0})


(defn focus-path [view-state]
  [:days (:day-in-focus view-state)
   :sessions (:session-in-focus view-state)
   (get [:task :start-time-hour :start-time-minute] (:session-column-in-focus view-state))])

(defn model-to-view-state [model view-state]
  (-> view-state
      (assoc :days (vec (map (fn [[child-view-model child-view-state]]
                               (day-view-model-to-day-view-state child-view-model child-view-state))
                             (partition 2 (interleave model
                                                      (or (:days view-state)
                                                          (repeat initial-day-view-state)))))))
      (as-> view-state
            (assoc-in view-state (concat (focus-path view-state) [:has-focus]) true))))


(defn view [state]
  (layout/->Stack [(drawable/->Rectangle 0
                                         0
                                         [1 1 1 1])
                   (margin 10 0 0 10
                           (let [style {:font (font/create "LiberationSans-Regular.ttf" 15)
                                        :foreground [0 0 0 1]}]
                             (apply vertically (->> (for-all [day-view-state (:days state)]
                                                             (day-view day-view-state style))
                                                    (interpose (drawable/->Empty 10 10))))))]))

(defn update-focus-state [new-state old-state]
  (-> new-state
      (assoc-in (concat (focus-path old-state) [:has-focus]) false)
      (assoc-in (concat (focus-path new-state) [:has-focus]) true)))


(defn handle-event [state event]
  #_(println "handling " event)
  (cond (events/key-pressed? event :down)
        (-> (if (< (:session-in-focus state)
                   (dec (count (-> state :days (get (:day-in-focus state)) :sessions))))
              (update-in state [:session-in-focus] inc)
              (if (< (:day-in-focus state)
                     (dec (count (:days state))))
                (-> state
                    (assoc-in [:session-in-focus] 0)
                    (update-in [:day-in-focus] inc))
                state))
            (update-focus-state state))

        (events/key-pressed? event :up)
        (-> (if (> (:session-in-focus state)
                   0)
              (update-in state [:session-in-focus] dec)
              (if (> (:day-in-focus state)
                     0)
                (-> state
                    (assoc-in [:session-in-focus] (dec (count (:sessions (get (:days state) (dec (:day-in-focus state)))))))
                    (update-in [:day-in-focus] dec))
                state))
            (update-focus-state state))


        (events/key-pressed? event :right)
        (-> (update-in state [:session-column-in-focus] inc)
            (update-focus-state state))

        (events/key-pressed? event :left)
        (-> (update-in state [:session-column-in-focus] dec)
            (update-focus-state state))

        (events/key-pressed? event :f1)
        (update-in state [:days (:day-in-focus state)]
                   (fn [day]
                     (-> (day-view-state-to-day-view-model day)
                         (update-in [:sessions] (fn [sessions]
                                                  (insert (vec sessions) (inc (:session-in-focus state)) (create-session))) )
                         (day-view-model-to-day-view-state day))))

        (events/key-pressed? event :f2)
        (update-in state [:days (:day-in-focus state)]
                   (fn [day]
                     (-> (day-view-state-to-day-view-model day)
                         (update-in [:sessions] (fn [sessions]
                                                  (remove-nth (vec sessions) (:session-in-focus state))))
                         (day-view-model-to-day-view-state day))))


        :default
        (update-in state (focus-path state)
                   #(handle-text-editor-event % event))))

(defonce event-queue (atom (event-queue/create)))

(defn start []
  (reset! event-queue (event-queue/create))
  #_(.start (Thread. (fn [] (start-view #'view
                                        @event-queue
                                        #'handle-event
                                        (model-to-view-state log initial-view-state)))))

  (start-view #'view
              @event-queue
              #'handle-event
              (model-to-view-state log initial-view-state)))

(event-queue/add-event @event-queue {})


;;(Start)

#_(let [layoutable (layout/->HorizontalStack [(layout/->Margin 10 10 10 10
                                                             [(drawable/->Text "Foo"
                                                                               (font/create "LiberationSans-Regular.ttf" 15)
                                                                               [1 1 1 1])])])
      layout (layout/layout layoutable 100 100)
      layoutable (layout/->HorizontalStack [(layout/->Margin 10 10 10 100
                                                             [(drawable/->Text "Foo"
                                                                               (font/create "LiberationSans-Regular.ttf" 15)
                                                                               [1 1 1 1])])])
      layout (layout/layout layoutable 100 100)]
  (println layout))


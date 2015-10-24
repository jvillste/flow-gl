(ns examples.drink-watch
  (:require [flow-gl.tools.trace :as trace]
            (flow-gl.gui [gui :as gui]
                         [controls :as controls]
                         [drawable :as drawable]
                         [layouts :as layouts]
                         [layoutable :as layoutable]
                         [layout :as layout])
            (flow-gl.graphics [font :as font])
            [clojure.string :as string]
            [clojure.java.io :as io]
            [flow-gl.utils :as utils]

            [flow-gl.gui.layout-dsl :as l]))

(defn time-to-minutes [[hour minutes]]
  (+ minutes (* 60 hour)))

(defn minutes-to-time [minutes]
  [(mod (int (/ minutes 60))
        24)
   (mod minutes 60)])

(defn event-x [min max width event]
  (* width
     (/ (- event min)
        (- max min))))

(defrecord TimeLine [min max events time-now]
  layout/Layout
  (layout [this application-state requested-width requested-height]
    (let [height 50]
      (layout/set-dimensions-and-layout (l/vertically (l/superimpose (drawable/->Rectangle requested-width height [100 100 100 255])
                                                                     (l/absolute (for [event events]
                                                                                   (-> (assoc (drawable/->Rectangle 5 height [255 255 255 255])
                                                                                              :x (event-x min
                                                                                                          max
                                                                                                          requested-width
                                                                                                          event)
                                                                                              :y 0)))
                                                                                 (assoc (drawable/->Rectangle 3 (/ height 2) [255 255 0 255])
                                                                                        :x (event-x min
                                                                                                    max
                                                                                                    requested-width
                                                                                                    time-now)
                                                                                        :y 0)))
                                                      
                                                      (l/absolute (for [event (take-while #(< % max)
                                                                                          (iterate #(+ % 60)
                                                                                                   min))]
                                                                    (assoc (controls/text (let [[hour minute] (minutes-to-time event)]
                                                                                            (str hour ":" minute)))
                                                                           :x (event-x min
                                                                                       max
                                                                                       requested-width
                                                                                       event)
                                                                           :y 0))))
                                        application-state
                                        0
                                        0
                                        requested-width
                                        requested-height)))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    {:width available-width
     :height 100}))

(defn time-now []
  [(.getHours (java.util.Date.))
   (.getMinutes (java.util.Date.))])

(defn drink-watch-view [view-context state]
  (-> (l/vertically (l/preferred (->TimeLine (time-to-minutes [19 0])
                                          (+ (* 60 24)
                                             (time-to-minutes [9 0]))
                                          (map time-to-minutes (:wakeups state))
                                          #_(+ (time-to-minutes [19 0])
                                             (* 10 (.getSeconds (java.util.Date.))))
                                          (time-to-minutes (time-now))))
                 (controls/button view-context "Juota" false (fn [local-state]
                                                               (let [local-state (update-in local-state [:wakeups] conj (time-now))]
                                                                 (spit "wakeups.clj" (:wakeups local-state))
                                                                 local-state))))
      (assoc :sleep-time (* 1000 60))))

(defn drink-watch [view-context]
  {:local-state {:wakeups (read-string (slurp "wakeups.clj"))}
   :view #'drink-watch-view})

(defonce event-channel (atom nil))

(trace/trace-some-from-ns 'examples.drink-watch)

(defn start []

  (reset! event-channel
          (gui/start-control drink-watch)
          
          #_(trace/with-trace
              (gui/start-control drink-watch))))


(when @event-channel
  (gui/redraw-app @event-channel))





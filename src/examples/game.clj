(ns examples.game
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer]
                         [transformers :as transformers])
            (flow-gl.opengl.jogl [quad :as quad]
                                 [render-target :as render-target]
                                 [opengl :as opengl])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [javax.media.opengl GL2])
  (:use flow-gl.utils
        clojure.test))





(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" 15)
                    color)))

(def car (buffered-image/create-from-file "/Users/jukka/Pictures/icons/1436407267_camaro_64.png"))
(def house (buffered-image/create-from-file "/Users/jukka/Pictures/icons/house_icon_by_yuninaa.jpg"))

(defn wave [time length from to]
  (+ (* (/ (Math/sin (* (/ (mod time length)
                           length)
                        Math/PI
                        2))
           2)
        (- to from))
     (/ (- to from)
        2)))

(println (for [time (range 0 10)]
           (wave time 10 0 10)))

(defn game-view [view-context state]
  #_(trace/log "blur raidus" (:frame-started view-context) #_(int (* (/ (mod (:frame-started view-context)
                                                                             1000)
                                                                        1000)
                                                                     4)))
  (let [tile-height 48
        tile-width 30
        player-color [255 0 0 255]
        obstacle-color [0 0 255 255]]
    (-> (l/superimpose (l/vertically (for [[row-number row] (indexed (:map state))]
                                       (l/horizontally (for [[column-number value]  (indexed row)]
                                                         (case value
                                                           nil (drawable/->Empty tile-width tile-height)
                                                           (-> (drawable/->Text (str value)
                                                                                (font/create "LiberationMono-Regular.ttf"
                                                                                             45)
                                                                                [#_(* (/ (mod (:frame-started view-context)
                                                                                              1000)
                                                                                         1000)
                                                                                      255)
                                                                                 255 0 0 255])
                                                               (assoc :width tile-width
                                                                      :height tile-height)
                                                               (cond-> (and (= row-number (:player-y state))
                                                                            (= column-number (:player-x state)))
                                                                 (assoc :transformer {:transformer (partial transformers/blur-transformer
                                                                                                            (int (wave (:frame-started view-context)
                                                                                                                       1000
                                                                                                                       0
                                                                                                                       5)))
                                                                                      :id :blur }))))))))
                       (l/absolute (assoc (drawable/->Rectangle tile-width tile-height [255 0 0 100])
                                          #_(drawable/->Image car)
                                          :x (* tile-width (:player-x state))
                                          :y (* tile-height (:player-y state)))))
        (assoc :sleep-time 10))))

(defn make-row [n]
  (let [values [nil]]
    (vec (repeatedly n (fn [] (get values
                                   (int (rand (dec (count values))))))))))

(defn clear-tile [state]
  (assoc-in state [:map  (:player-y state) (:player-x state)] 0))

(defn handle-keyboard-event [local-state event]
  (println event)
  (if (= (:type event)
         :key-pressed)
    (if-let [character (:character event)]
      (assoc-in local-state [:map  (:player-y local-state) (:player-x local-state)] character)
      (-> (cond
            (events/key-pressed? event :down)
            (update-in local-state [:player-y] (fn [player-y] (min (dec (count (:map local-state)) )
                                                                   (inc player-y))))
            (events/key-pressed? event :up)
            (update-in local-state [:player-y] (fn [player-y] (max 0
                                                                   (dec player-y))))
            (events/key-pressed? event :right)
            (update-in local-state [:player-x] (fn [player-x] (min (dec (count (first (:map local-state))))
                                                                   (inc player-x))))
            (events/key-pressed? event :left)
            (update-in local-state [:player-x] (fn [player-x] (max 0
                                                                   (dec player-x))))

            :default
            local-state)
          #_(clear-tile)))
    local-state))

(defn game [view-context]
  {:local-state {:player-x 0
                 :player-y 0
                 :map (vec (repeatedly 6 (fn [] (make-row 10))))}
   :handle-keyboard-event-with-local-state #'handle-keyboard-event
   :can-gain-focus true
   :view #'game-view})

(defonce event-channel (atom nil))

(defn start []
  #_(.start (Thread. (fn []
                       (trace/untrace-ns 'flow-gl.gui.gui)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus-if-can-gain-focus)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-size-dependent-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))

  (reset! event-channel
          #_(gui/start-control game)
          (trace/with-trace
            (gui/start-control game)))
  

  #_(profiler/with-profiler (gui/start-control barless-root)))


(when @event-channel
  (gui/redraw-app @event-channel))


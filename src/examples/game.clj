(ns examples.game
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
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

(defn game-view [view-context state]
  (let [tile-size 64
        player-color [255 0 0 255]
        obstacle-color [0 0 255 255]]
    (l/superimpose (l/vertically (for [row (:map state)]
                                   (l/horizontally (for [value row]
                                                     (if (= 0 value)
                                                       
                                                       (drawable/->Empty tile-size tile-size)
                                                       
                                                       (drawable/->Image house))))))
                   (l/absolute (assoc (drawable/->Image car)
                                      :x (* tile-size (:player-x state))
                                      :y (* tile-size (:player-y state)))))))

(defn make-row [n]
  (repeatedly n (fn [] (int (rand 2)))))

(defn game [view-context]
  {:local-state {:player-x 0
                 :player-y 0
                 :map (repeatedly 20 (fn [] (make-row 20)))}
   :handle-keyboard-event (fn [state event]
                            (println "key" event)
                            (cond
                              (events/key-pressed? event :down)
                              (gui/apply-to-local-state state view-context (fn [local-state]
                                                                             #_(if (= 0 (-> (:map local-state)
                                                                                            (nth ))))
                                                                             (update-in local-state [:player-y] (fn [player-y] (min (count (:map local-state))
                                                                                                                                    (inc player-y))))) )
                              (events/key-pressed? event :up)
                              (gui/apply-to-local-state state view-context (fn [local-state]
                                                                             (update-in local-state [:player-y] (fn [player-y] (max 0
                                                                                                                                    (dec player-y))))) )
                              (events/key-pressed? event :right)
                              (gui/apply-to-local-state state view-context (fn [local-state]
                                                                             (update-in local-state [:player-x] (fn [player-x] (min (count (first (:map local-state)))
                                                                                                                                    (inc player-x))))) )
                              (events/key-pressed? event :left)
                              (gui/apply-to-local-state state view-context (fn [local-state]
                                                                             (update-in local-state [:player-x] (fn [player-x] (max 0
                                                                                                                                    (dec player-x))))) )

                              :default
                              state))
   :can-gain-focus true
   :view #'game-view})


(defn start []
  #_(.start (Thread. (fn []
                       (trace/untrace-ns 'flow-gl.gui.gui)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus-if-can-gain-focus)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-size-dependent-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))
  
  (.start (Thread. (fn []
                     (gui/start-control game))))

  #_(profiler/with-profiler (gui/start-control barless-root)))



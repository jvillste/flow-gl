(ns examples.flow-layout
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events]
                         [view :as view])

            (flow-gl.gui.components [text-editor :as text-editor]
                                    [button :as button])

            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]))
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl))

(defn random-text []
  (->> (repeatedly #(char (+ (rand (- (int \z)
                                      (int \a)))
                             (int \a))))
       (take (rand 15))
       (apply str)))

(view/def-view todo-list [state]
  {:texts (repeatedly 10 random-text)
   :handle-keyboard-event (fn [state event]
                            (cond
                             (events/key-pressed? event :esc)
                             (assoc state :close-requested true)

                             :default
                             state))}

  (layout/->VerticalStack (for-all [text (:todo-texts state)]
                                   (do (println "text " text)
                                       (drawable/->Text text
                                                        (font/create "LiberationSans-Regular.ttf" 15)
                                                        [1 1 1 1])))))

(def event-queue (atom (event-queue/create)))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (view/start-view @event-queue
                                           todo-list)))))

(event-queue/add-event @event-queue {})

(ns examples.drag-and-drop
  (:require [clojure.core.async :as async]
            [flow-gl.tools.layoutable-inspector :as layoutable-inspector]
            [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view]
                         [quad-gui :as quad-gui])

            (flow-gl.graphics [command :as command]
                              [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
            [clj-http.client :as client])
  (:use flow-gl.utils
        flow-gl.gui.layout-dsl
        clojure.test))

(quad-gui/def-view view [state]
  (layout/->Absolute [(assoc (drawable/->Rectangle 50 50 [1 1 1 1])
                        :x 0
                        :y (:position state)
                        :on-drag (fn [state x y]
                                   (update-in state [:position] + y)))]))

(defn create [state-path event-channel control-channel]
  (conj {:position 100
         :handle-keyboard-event (fn [state event]
                                  (events/on-key state event
                                                 :esc (do (quad-gui/request-close event-channel)
                                                          state)))}
        {} #_quad-gui/child-focus-handlers))

(def root
  {:constructor create
   :view view})

(defn start []
  (quad-gui/start-view root))


(run-tests)

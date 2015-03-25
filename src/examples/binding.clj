(ns examples.binding
  (:require [clojure.core.async :as async]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad :as quad])
            (flow-gl.tools [profiler :as profiler])
            [datomic.api :as d]
            (flow-gl.gui [drawable :as drawable]
                         [gui :as gui]
                         [transformer :as transformer]
                         [renderer :as renderer]
                         [window :as window]
                         [layout :as layout]
                         [layouts :as layouts]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            [flow-gl.debug :as debug])
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable]
           [java.nio ByteBuffer])
  (:use flow-gl.utils
        midje.sweet
        clojure.test))

(defn text [value]
  (drawable/->Text (str value)
                   (font/create "LiberationSans-Regular.ttf" 25)
                   [255 255 255 255]))

(defn counter [view-context]
  {:view (fn [view-context state]
           (-> (text (:count state))
               (gui/add-mouse-event-handler (fn [state event]
                                              (if (= (event :type)
                                                     :mouse-clicked)
                                                (let [increment (case [(event :key) (event :shift)]
                                                                  [:left-button false] 1
                                                                  [:left-button true] 10
                                                                  [:right-button false] -1
                                                                  [:right-button true] -10)]
                                                  (gui/update-binding state
                                                                      view-context
                                                                      (fn [old-value] (+ old-value increment))
                                                                      :count))
                                                state)))))})

(defn app [view-context]
  {:local-state {:count-1 0
                 :count-2 0
                 :text ""}
   :view (fn [view-context state]
           (l/vertically (text (str (:text state) ":" (+ (:count-1 state)
                                                         (:count-2 state))))
                         (gui/call-and-bind view-context
                                            state
                                            :count-1
                                            :count
                                            counter
                                            :counter-1)

                         (gui/call-and-bind view-context
                                            state
                                            :count-2
                                            :count
                                            counter
                                            :counter-2)

                         (gui/call-and-bind view-context
                                            state
                                            :text
                                            :text
                                            controls/text-editor
                                            :text-editor)))})

(defn start []
  (.start (Thread. (fn []
                     (profiler/with-profiler (gui/start-control app)))))
  #_(.start (Thread. (fn [] (gui/start-control app)))))

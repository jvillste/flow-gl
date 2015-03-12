(ns examples.keyboard
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

;; Control test

(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 25)
                      color)))

(defn counter-view [view-context state]
  (l/preferred (text (:count state)
                     (if (:has-focus state)
                       [0 255 0 255]
                       [100 100 100 255]))))

(defn counter [view-context]
  {:count 0
   :can-gain-focus true
   :handle-keyboard-event (fn [state event]
                            [(update-in state [:count] inc)
                             true])
   :view #'counter-view})

(defn app [view-context]
  (merge gui/child-focus-handlers
         {:view (fn [view-context state]
                  (l/preferred (l/horizontally (for [column (range 10)]
                                                 (gui/call-view view-context counter column)))))}))
(defn start []
  (.start (Thread. (fn [] (gui/start-control app))))

  #_(.start (Thread. (fn [] (profiler/with-profiler (gui/start-control app)))))
  #_(profiler/with-profiler (gui/start-control app)))

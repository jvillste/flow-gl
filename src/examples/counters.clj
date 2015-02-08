(ns examples.counters
  (:require [clojure.core.async :as async]
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [quad :as quad])
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

(defn counter [view-context]
  {:count 0
   :view (fn [view-context state]
           (gui/on-mouse-clicked (text (:count state)
                                       (if (:mouse-over state)
                                         [255 255 255 255]
                                         [100 100 100 255]))
                                 view-context
                                 (fn [state event]
                                   (update-in state [:count] inc))))})

(defn app [view-context]
  {:view (fn [view-context state]
           (l/horizontally (for-all [column (range 10)]
                                    (l/vertically (for-all [row (range 10)]
                                                           (gui/call-view view-context counter [row column]))))))})


(defn start []
  (gui/start-control app))

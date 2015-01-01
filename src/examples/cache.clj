(ns examples.cache
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
                         [layout-dsl :as l]
                         )
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


(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))

(defn cache [layoutable]
  (transformer/with-transformers
    (transformer/->Cache :cache)
    layoutable))

(defn counter-view [view-context state]
  (gui/on-mouse-clicked (l/horizontally (repeat (:count state) (text "X")))
                        view-context
                        (fn [state event]
                          (update-in state [:count] inc))))

(defn counter [view-context]
  {:count 10
   :view #'counter-view})

(defn app-view [view-context state]
  (l/vertically (for-all [x (range 2)]
                         (gui/call-view view-context counter (keyword (str x))))))

(defn app [view-context]

  {:view #'app-view})

(gui/redraw-last-started-view)

(defn start []
  (.start (Thread. (fn [] (profiler/with-profiler (gui/start-control app)))))
  
  #_(.start (Thread. (fn [] (gui/start-control app)))))



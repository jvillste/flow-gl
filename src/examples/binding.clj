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

;; Control test

(defn counter-mouse-handler [state event view-context]
  (let [local-state (gui/get-view-state state view-context)]
    ((:on-change local-state)
     state
     (inc (:count local-state)))))

(defn counter-view [view-context state]
  (-> (drawable/->Text (:count state)
                       (font/create "LiberationSans-Regular.ttf" 25)
                       [255 255 255 255])
      (gui/on-mouse-clicked-2 counter-mouse-handler view-context)))

(defn counter [view-context]
  {:local-state {:count 0}
   :view counter-view})

(defn app [view-context]
  {:local-state {:count 0}
   :view (fn [view-context state]
           (l/preferred (-> (gui/call-view view-context counter :counter)
                            (gui/bind view-context state :count :count))))})

(defn start []
  #_(gui/start-control app)
  (.start (Thread. (fn [] (gui/start-control app))))
  #_(.start (Thread. (fn [] (profiler/with-profiler (gui/start-control app)))))
  #_(profiler/with-profiler (gui/start-control app)))

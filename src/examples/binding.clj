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
                                              (if (= :mouse-clicked (:type event))
                                                (let [function (if (= :left-button (:key event))
                                                                 (if (:shift event)
                                                                   (fn [value] (+ value 10))
                                                                   inc)
                                                                 (if (:shift event)
                                                                   (fn [value] (- value 10))
                                                                   dec))]
                                                  (gui/update-binding state view-context function :count))
                                                state)))))})

(defn app [view-context]
  {:local-state {:app-count-1 0
                 :app-count-2 0}
   :view (fn [view-context state]
           (l/vertically (text (str "total:" (+ (:app-count-1 state)
                                                (:app-count-2 state))))
                         (-> (gui/call-view view-context counter :counter-1)
                             (gui/bind view-context state :app-count-1 :count))
                         
                         (-> (gui/call-view view-context counter :counter-2)
                             (gui/bind view-context state :app-count-2 :count))))})

(defn start []
  (.start (Thread. (fn [] (gui/start-control app)))))
















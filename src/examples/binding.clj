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

(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 25)
                      color)))

(defn counter-mouse-handler [state state-path event]
  (let [local-state (get-in state state-path)]
    (apply (:on-change local-state)
           state
           (inc (:count local-state))
           (:arguments local-state))))

(flow-gl.debug/defn-timed counter-view
  #_""
  #_{:name "counter-view"}
  [view-context state]
  (-> (text (apply str (if (= 0 (mod (:count state) 2))
                         "X"
                         "ZZ"))
            (if (:mouse-over state)
              [0 255 0 255]
              [100 100 100 255]))
      (gui/on-mouse-clicked view-context
                            counter-mouse-handler)))

(defn counter [view-context]
  {:local-state {:count 0}
   :view counter-view})

(defn highlight [element highlight? color]
  (if highlight?
    (l/box 0 (drawable/->Rectangle 0 0 color)
           element)
    element))

(defn mouse-enter [state event row column]
  (assoc state
    :mouse-over-row row
    :mouse-over-column column))

(defn on-change [state new-count view-context]
  (gui/apply-to-view-state state
                           view-context
                           assoc :count new-count))

(defn app [view-context]
  {:local-state {:count 0}
   :handle-keyboard-event (fn [state event]
                            [(update-in state [:count] inc)
                             true])
   :view (fn [view-context state]
           (l/preferred (gui/call-view view-context counter :counter {:count (:count state)
                                                                      :on-change on-change
                                                                      :arguments view-context})))})

(defn start []
  #_(gui/start-control app)
  #_(.start (Thread. (fn [] (profiler/with-profiler (gui/start-control app)))))
  (profiler/with-profiler (gui/start-control app)))

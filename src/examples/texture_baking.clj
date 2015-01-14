(ns examples.texture-baking
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
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))

(defn app [view-context]
  (async/go-loop []
    (async/alt! (:control-channel view-context) ([_] (println "exiting counter process"))
                (async/timeout 2000) ([_]
                                        (gui/apply-to-state view-context update-in [:count] inc)
                                        (recur))))

  {:count 0
   :view (fn [view-context state]
           (l/vertically (text "Foo")
                         (text (:count state))
                         (text "Bar")))})


(defn layout [layoutable]
  (-> layoutable
      (layout/layout {} Integer/MAX_VALUE Integer/MAX_VALUE)
      (second)
      (assoc :x 0 :y 0 :z 0)))




(defn start []
  #_(gui/start-app layout-app)
  (gui/start-control app)
  #_(gui/start-control static-app))


(defn view [count]
  (l/vertically (l/horizontally (text "Foo") (text "Bar"))
                (text count)
                (text "Bar")))

(clojure.pprint/pprint (gui/partition-by-differences (layout (view 0))
                               (layout (view 1))))




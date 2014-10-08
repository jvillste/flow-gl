(ns examples.hello-gui
  (:require [clojure.core.async :as async]
            [com.climate.claypoole :as claypoole]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l])
            [flow-gl.debug :as debug]
            [flow-gl.tools.debug-monitor :as debug-monitor]
            [flow-gl.csp :as csp]
            [clojure.string :as string]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]))
  (:import [java.io File]
           [java.util.concurrent Executors]
           [java.lang Runnable]
           [java.util Random])
  (:use flow-gl.utils
        clojure.test))

(def image (buffered-image/create-from-file "pumpkin.png"))

(gui/def-control animation
  ([view-context control-channel]
     {})

  ([view-context state]
     (layouts/->Preferred (drawable/->Image image)
                          #_(drawable/->Rectangle 100 100 [255 1 1 255]))))

#_(debug/reset-log)
(defn start []
  (gui/start-view #'create-animation #'animation-view))

#_(gui/redraw-last-started-view)

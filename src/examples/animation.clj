(ns examples.animation
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
        ;;    [flow-gl.tools.debug-monitor :as debug-monitor]
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



(gui/def-control animation
  ([view-context control-channel]
     {:target-position :left})

  ([view-context {:keys [target-position] :as state}]
     (let [value (-> (stoppable-animation-phase state :animation repeat 6000)
                     (linear 100 200)
                     (float))]
       (layouts/->Preferred (-> (drawable/->Rectangle value value [(if (stoppable-animation-running state :animation)
                                                                     0
                                                                     1) 1 1 1])
                                (gui/on-mouse-clicked view-context
                                                           (fn [state time]
                                                             (toggle-stoppable-animation state :animation time))))))))


#_(debug/reset-log)
(defn start []
  (debug-monitor/with-debug-monitor
    (.start (Thread. (fn []
                       (gui/start-view #'create-animation #'animation-view))))))

(gui/redraw-last-started-view)

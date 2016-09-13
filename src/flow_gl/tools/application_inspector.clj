(ns flow-gl.tools.application-inspector
  (:require [clojure.core.async :as async]
            [flow-gl.opengl.jogl.window :as jogl-window]
            (flow-gl.gui [drawable :as drawable]
                         [window :as window]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
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
        clojure.test))

(defn text
  ([value]
     (text value [255 255 255 255]))

  ([value color]
     (drawable/->Text (str value)
                      (font/create "LiberationSans-Regular.ttf" 15)
                      color)))

(defn layoutable-view [layoutable layout-path state]
  (text (layoutable/layoutable-name layoutable)
        (if (= layout-path (last (:layout-paths-under-mouse state)))
          [255 255 255 255]
          [200 200 200 255])))

(defn add-indexes [sequence]
  (partition 2 (interleave (iterate inc 0) sequence)))

(defn layout-view [layout layout-path state]
  (l/vertically
   (when layout (layoutable-view layout layout-path state))
   (when (:children layout)
     (l/margin 0 0 0 10
               (l/vertically
                (for-all [[index layout] (add-indexes (:children layout))]
                         (layout-view layout
                                      (concat layout-path [:children index])
                                      state)))))))

#_(def layout (layout/do-layout (l/horizontally (text "foo") (text "bar"))))

(defn application-inspector [control]
  (fn [view-context]
    {:view (fn [view-context state]
             (let [application-state (-> view-context :application-state)]
               (l/vertically (gui/call-view view-context control :control)
                             (controls/text (->> (:layout-paths-under-mouse application-state)
                                                 (map (fn [path]
                                                        (type (get-in application-state (concat [:layout] path)))))
                                                 vec))
                             (controls/text (-> view-context :application-state :focused-state-paths vec))
                             #_(layout-view (-> view-context :application-state :layout) [:view-state] (-> view-context :application-state) ))))}) )

#_(defn start-view []
    (gui/start-view #'create-layout-inspector #'layout-inspector-view))

#_(defn start []
    (.start (Thread. (fn []
                       (start-view)))))

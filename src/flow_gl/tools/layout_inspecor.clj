(ns flow-gl.tools.layout-inspector
  (:require [clojure.core.async :as async]
            [flow-gl.opengl.jogl.window :as jogl-window]
            [datomic.api :as d]
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
        (if (= layout-path (:layout-path-under-mouse state))
          [255 255 255 255]
          [200 200 200 255])))

(defn add-indexes [sequence]
  (partition 2 (interleave (iterate inc 0) sequence)))

(defn layout-view [layout layout-path state]
  (l/vertically
   (layoutable-view layout layout-path state)
   (when (:children layout)
     (l/margin 0 0 0 10
               (l/vertically
                (for-all [[index layout] (add-indexes (:children layout))]
                         (layout-view layout
                                      (concat layout-path [:children index])
                                      state)))))))


(def layout (layout/do-layout (l/horizontally (text "foo") (text "bar"))))


(gui/def-control layout-inspector
  ([view-context control-channel]
     {})

  ([view-context state]
     (layout-view layout [] {:layout-path-under-mouse [:children 0]})))

(defn start-view []
  (gui/start-view #'create-layout-inspector #'layout-inspector-view))

(defn start []
  (.start (Thread. (fn []
                     (start-view)))))


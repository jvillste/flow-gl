(ns flow-gl.examples.window
  (:require [clojure.core.async :as async]
            [flow-gl.opengl.jogl.window :as jogl-window]
            [flow-gl.opengl.jogl.opengl :as opengl]

            [datomic.api :as d]
            (flow-gl.gui [drawable :as drawable]
                         [renderer :as renderer]
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

(defn start []
  (let [window (jogl-window/create 300
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :reshape opengl/resize)
        nanovg-renderer (window/with-gl window gl
                          (renderer/create-nanovg-renderer))]


    (try
      (window/render-constantly window gl
                                (renderer/render-frame [(assoc (drawable/->Rectangle 100 100 [255 255 255 255])
                                                          :x 0 :y 0)]
                                                       gl
                                                       [nanovg-renderer]))

      (Thread/sleep 400)
      (window/close window)

      (catch Exception e
        (window/close window)
        (throw e)))))

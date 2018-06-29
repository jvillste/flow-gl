(ns examples.scrolling
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            (flow-gl.opengl.jogl [quad :as quad]
                                 [render-target :as render-target]
                                 [opengl :as opengl])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

(defn text [value]
  (layouts/->Margin 2 2 0 0
                    [(drawable/->Text (str value)
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      [255 255 255 255])]))

(defn scrolling-text-view [view-context state]
  (l/margin 10 10 10 10
            (gui/call-view controls/scroll-panel
                           :scroll-panel
                           {:content (l/vertically (for [i (range 20)]
                                                     (controls/text (str "as fasf asdf asf asdf asdf ads faas fas fasdf" i))))})))

(defn barless-root [view-context]
  {:view #'barless-root-view})


(defn start []
  #_(.start (Thread. (fn []
                       (trace/trace-ns 'flow-gl.gui.gui)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))

  (gui/start-control barless-root)

  #_(profiler/with-profiler (gui/start-control barless-root)))


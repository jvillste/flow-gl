(ns examples.size-dependent
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable])
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

(defn root-view [view-context state]
  (layouts/->SizeDependent (fn [child requested-width requested-height]
                             (let [{preferred-width :width preferred-height :height} (layoutable/preferred-size child requested-width requested-height)]
                               (l/superimpose (l/preferred child)
                                              (l/absolute (when (< requested-height preferred-height)
                                                            (assoc (drawable/->Rectangle 10 requested-height [255 0 0 100])
                                                              :x (- requested-width 10)
                                                              :y 0))
                                                          (when (< requested-width preferred-width)
                                                            (assoc (drawable/->Rectangle requested-width 10 [255 0 0 100])
                                                              :x 0
                                                              :y (- requested-height 10)))))))
                           [(drawable/->Rectangle 300 300 [0 255 255 255])]))

(defn root [view-context]
  {:view #'root-view})

#_(flow-gl.debug/set-active-channels :all)

(defn start []
  #_(.start (Thread. (fn []
                       (trace/trace-ns 'flow-gl.gui.gui)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-view-calls)
                       (trace/with-trace
                         (gui/start-control root)))))

  (.start (Thread. (fn []
                     (gui/start-control root))))

  #_(profiler/with-profiler (gui/start-control root)))



#_(run-tests)

;; flow-gl.debug/debug-timed

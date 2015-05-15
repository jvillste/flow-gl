(ns examples.transformers
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            [flow-gl.opengl.jogl.quad :as quad]
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

(defn barless-root-view [view-context state]
  (l/margin 50 50 50 50 (-> (l/vertically (for [i (range 20)]
                                            (-> (controls/text (str "as fasf asdf asf asdf asdf ads faas fas fasdf" i))
                                                (assoc :transformer {:id [:text-transformer i] 
                                                                     :transformer (fn [layout gpu-state]
                                                                                    [(assoc (l/superimpose (assoc layout
                                                                                                                    :x 0 :y 0)
                                                                                                             (assoc (drawable/->Rectangle (:width layout)
                                                                                                                                          (:height layout)
                                                                                                                                          [255 0 0 50])
                                                                                                                    :x 0 :y 0) )
                                                                                              :x (:x layout)
                                                                                              :y (:y layout)
                                                                                              :width (:width layout)
                                                                                              :height (:height layout))
                                                                                     gpu-state])}))))
                            (assoc :transformer {:id :transformer-1
                                                 :transformer (fn [layout gpu-state]
                                                                [(assoc (l/superimpose (assoc layout
                                                                                                :x 0 :y 0)
                                                                                         (assoc (drawable/->Rectangle (:width layout)
                                                                                                                      (:height layout)
                                                                                                                      [255 255 0 100])
                                                                                                :x 0 :y 0) )
                                                                          :x (:x layout)
                                                                          :y (:y layout)
                                                                          :width (:width layout)
                                                                          :height (:height layout))
                                                                 gpu-state])}))))

(defn barless-root [view-context]
  {:view #'barless-root-view})


(defn start []
  #_(.start (Thread. (fn []
                       (trace/trace-ns 'flow-gl.gui.gui)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))

  (.start (Thread. (fn []
                     (gui/start-control barless-root))))

  #_(profiler/with-profiler (gui/start-control barless-root)))


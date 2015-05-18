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

(defn barless-root-view [view-context state]
  (l/margin 10 10 10 10 (-> (l/vertically (for [i (range 20)]
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
                                                                (let [gl (:gl gpu-state)
                                                                      state (or (get gpu-state :transformer-1)
                                                                                {})
                                                                      width (:width layout)
                                                                      height (:height layout)
                                                                      render-target (if-let [render-target (:render-target state)]
                                                                                      (if (and (= width
                                                                                                  (:width render-target))
                                                                                               (= height
                                                                                                  (:height render-target)))
                                                                                        render-target
                                                                                        (do (render-target/delete render-target gl)
                                                                                            (render-target/create width height gl)))
                                                                                      (render-target/create width height gl))
                                                                      state (assoc state :render-target render-target)
                                                                      gpu-state (render-target/render-to render-target gl
                                                                                                         (opengl/clear gl 0 0 0 1)
                                                                                                         (-> (assoc gpu-state :drawables (gui/drawables-for-layout (assoc layout
                                                                                                                                                                          :x 0
                                                                                                                                                                          :y 0)))
                                                                                                             (gui/render-drawables)))]
                                                                  
                                                                  [(drawable/->Quad ["texture" (:texture render-target)]
                                                                                    [:1f "resolution" width
                                                                                     :1f "radius" 1
                                                                                     :2f "dir" [1.0 0.0]]
                                                                                    quad/blur-fragment-shader-source
                                                                                    (:x layout) (:y layout) width height)
                                                                   (assoc gpu-state :transformer-1 state)]))}))))

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


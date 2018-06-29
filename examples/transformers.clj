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
                                            (text (str "aas fasf asdf asf asdf asdf ads faas fas fasdf" i))))

                            (assoc :transformer {:id :transformer-1
                                                 :transformer (fn [layout gpu-state state]
                                                                (flow-gl.tools.trace/log "transformer state" state)
                                                                (let [gl (:gl gpu-state)
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
                                                                                     :1f "radius" 0.4
                                                                                     :2f "dir" [1.0 0.0]]
                                                                                    quad/blur-fragment-shader-source
                                                                                    (:x layout) (:y layout) width height)
                                                                   gpu-state
                                                                   state]))}))))

(defn barless-root [view-context]
  {:view #'barless-root-view})


(defn start []
  #_(.start (Thread. (fn []
                       (trace/trace-ns 'flow-gl.gui.gui)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-view-calls)
                       (trace/with-trace
                         (gui/start-control barless-root)))))

  #_(gui/start-control barless-root)

  #_(trace/with-trace
      (gui/start-redrawable-control barless-root))

  (gui/start-redrawable-control barless-root))

(gui/redraw-last-started-redrawable-control)


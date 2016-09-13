(ns examples.render-drawables
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            [flow-gl.tools.trace :as trace]
            [flow-gl.csp :as csp]
            [flow-gl.profiling :as p]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [controls :as controls]
                         [transformers :as transformers]
                         [animation :as animation]
                         [cache :as cache])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
            [flow-gl.gui.layout-dsl :as l]
            [clj-http.client :as client])
  (:use flow-gl.utils
        
        clojure.test))

(def table
  (l/table 5
           (for [y (range 2)]
             (for [x (range 2)]
               (controls/text (str x "," y))))))

(defn create-layout []
  (second (layout/do-layout (l/preferred table)
                            {}
                            100
                            100
                            (cache/create))))

(defn test []
  (println "start")
  (let [layout (time (assoc (create-layout)
                            :x 0 :y 0 :z 0))]
    (time (gui/drawables-for-layout layout))
    nil))

(defn root-view [view-context state]
  (l/preferred (l/table 5
                        (for [y (range 20)]
                          (for [x (range 20)]
                            (-> (gui/call-view controls/text-editor [:editor x y])
                                (update-in [:state-overrides]
                                           assoc
                                           :text (or (get state [x y]) "empty"))
                                (update-in [:constructor-overrides]
                                           assoc [:on-change :text]
                                           (fn [global-state new-value]
                                             (gui/apply-to-local-state global-state
                                                                       view-context
                                                                       (fn [state]
                                                                         (assoc state [x y] new-value))))))
                            #_(controls/text (str x "," y)))))))

(defn root [view-context]
  {:local-state {}
   :view #'root-view})


#_(trace/untrace-ns 'flow-gl.gui.gui)
(trace/trace-ns 'examples.render-drawables)

(trace/untrace-ns 'flow-gl.gui.layout)

(trace/trace-ns 'flow-gl.gui.gui)
(trace/trace-var 'gui/render-drawables-with-renderers)

(trace/untrace-ns 'flow-gl.gui.renderer)
(trace/trace-var 'flow-gl.gui.renderer/render-drawables-with-renderers)
(trace/trace-var 'flow-gl.gui.renderer/group-by-renderers)


#_(trace/trace-var 'gui/add-layout-afterwards)
#_(trace/trace-var 'gui/drawables-for-layout)
#_(trace/trace-var 'flow-gl.gui.gui/children-to-vectors)
#_(trace/trace-var 'flow-gl.gui.gui/resolve-size-dependent-view-calls)
#_(trace/trace-var 'flow-gl.gui.gui/run-view)



(defn start []
  #_(gui/start-redrawable-control root)


  (p/unprofile-ns 'flow-gl.gui.gui)
  (p/unprofile-ns 'flow-gl.gui.renderer)

  
  (gui/start-control root)
  #_(trace/with-trace
      (gui/start-control root))

  #_(test)
  #_(test)
  
  #_(trace/with-trace
      (test)
      (test))
  )


#_(gui/redraw-last-started-redrawable-control)

#_(run-tests)

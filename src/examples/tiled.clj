(ns examples.tiled
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            [flow-gl.tools.trace :as trace]
            [flow-gl.csp :as csp]
            [flow-gl.gui.components.static-table :as static-table]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [controls :as controls]
                         [transformers :as transformers]
                         [animation :as animation])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
            [flow-gl.gui.layout-dsl :as l]
            [clj-http.client :as client])
  (:use flow-gl.utils
        midje.sweet
        
        clojure.test))

(def table (-> (l/table 5
                        (for [y (range 20)]
                          (for [x (range 20)]
                            (controls/text (str x "," y)))))
               (assoc :transformer (assoc transformers/cache
                                          :id :cache))))

(defn table-view [view-context state]
  
  (let [rows (for [y (range 40)]
               (for [x (range 40)]
                 (-> (controls/text (str x "," y ":" (or (get state [x y])
                                                         0)))
                     (gui/on-mouse-clicked-with-view-context view-context
                                                             (fn [state event]
                                                               (println "clicked" x " " y " " state)
                                                               (update-in state [[x y]] (fn [count] (inc (or count 0)))))))))]
    (gui/call-view static-table/static-table
                   :static-table
                   {:rows rows}
                   [rows]))
  
  #_(-> (l/table 5
                 (for [y (range 40)]
                   (for [x (range 40)]
                     (-> (controls/text (str x "," y ":" (or (get state [x y])
                                                             0)))
                         (gui/on-mouse-clicked-with-view-context view-context
                                                                 (fn [state event]
                                                                   (println "clicked" x " " y " " state)
                                                                   (update-in state [[x y]] (fn [count] (inc (or count 0))))))))))
        (assoc :transformer (assoc transformers/cache
                                   :id :cache))))

(defn table-component [view-context]
  {:local-state {}
   :view #'table-view})

(defn table-function []
  (l/table 5
           (for [y (range 50)]
             (for [x (range 50)]
               (controls/text (str x "," y))))))




(defn view [frame-started]
  (l/preferred (-> (l/fixed-size 200 200
                                 (-> (l/absolute
                                      (-> (l/vertically table) #_(table-function) 
                                          (assoc :x (int (animation/wave frame-started
                                                                         2000
                                                                         0
                                                                         100)) 
                                                 :y 0)))))
                   (assoc :transformer (assoc transformers/clip
                                              :id :clip)))))

(defn scroll-view []
  (l/preferred (-> #_(l/preferred (gui/call-view table-component
                                               :table))
                   (l/fixed-size 2000 2000
                                   #_(gui/call-view controls/scroll-panel
                                                    :scroll-panel
                                                    {:content (l/vertically (gui/call-view table-component
                                                                                           :table))})
                                   (gui/call-view table-component
                                                  :table))
                   (assoc :transformer (assoc transformers/clip
                                              :id :clip)))))


(defn root-view [view-context state]
  (scroll-view)
  #_(assoc (scroll-view)
           :sleep-time 1000)
  #_(assoc (view (:frame-started view-context))
           :sleep-time 10))

(defn root [view-context]
  {:local-state {}
   :view #'root-view})


(trace/untrace-ns 'flow-gl.gui.gui)
(trace/trace-ns 'flow-gl.gui.layout)
(trace/untrace-var 'flow-gl.gui.layout/find-layoutable-paths)
(trace/trace-var 'flow-gl.gui.gui/resolve-view-call)
(trace/trace-var 'flow-gl.gui.gui/resolve-view-calls)
(trace/trace-var 'flow-gl.gui.gui/run-view-call)
(trace/trace-var 'examples.tiled/table-view)
(trace/trace-var 'flow-gl.gui.components.static-table/static-table)
(trace/trace-var 'flow-gl.gui.layouts/static-table-layout)

#_(trace/trace-var 'flow-gl.gui.gui/children-to-vectors)
#_(trace/trace-var 'flow-gl.gui.gui/add-layout-paths)
#_(trace/trace-var 'flow-gl.gui.gui/resolve-size-dependent-view-calls)
#_(trace/trace-var 'flow-gl.gui.gui/run-view)



(defn start []
  #_(gui/start-redrawable-control root)

  (trace/with-trace
    (gui/start-control root)
    )
  
  #_(.start (Thread. (fn []
                       (trace/trace-ns 'examples.autocompleter)
                       #_(trace/untrace-ns 'examples.autocompleter)
                       (trace/with-trace
                         (gui/start-control root))))))


(gui/redraw-last-started-redrawable-control)

#_(run-tests)

(ns fungl.application-test
  (:require [fungl.application :as application]
            [clojure.test :require [deftest is]]
            [fungl.layouts :as layouts]
            [fungl.layout :as layout]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.cache :as cache]
            [flow-gl.tools.trace :as trace]
            [fungl.view-compiler :as view-compiler]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.pprint :as pprint]))

(defn text [string]
  {:string string
   :width (count string)
   :height 1})

(def value-atom (atom {}))

(comment
  (do (trace/untrace-ns 'fungl.layout)
      (trace/trace-ns 'fungl.layout))


  (trace/trace-var #'layout/do-layout)
  (trace/untrace-var #'layout/do-layout)


  (get @value-atom 1526611343)

  ;;  TODO: why these print the same call tree while the later one should be cached?
  (with-bindings (cache/state-bindings)
    (trace/with-call-tree-printing value-atom
      (scene-graph/leaf-nodes (layout/do-layout-for-size (layouts/vertically-2 {:margin 1}
                                                                               (text "foo")
                                                                               (text "bar")
                                                                               (layouts/horizontally-2 {:margin 1}
                                                                                                       (text "foo")
                                                                                                       (text "bar")))
                                                         100
                                                         100)))
    (println "cache size:" (cache/size))


    (trace/with-call-tree-printing value-atom
      (scene-graph/leaf-nodes (layout/do-layout-for-size (layouts/vertically-2 {:margin 1}
                                                                               (text "foo")
                                                                               (text "bar")
                                                                               (layouts/horizontally-2 {:margin 1}
                                                                                                       (text "foo")
                                                                                                       (text "bar")))
                                                         100
                                                         100)))


    (println "cache size:" (cache/size))

    )

  (hash (layouts/horizontally-2 {:margin 1}
                                (text "foo")
                                (text "bar")))
  (application/start-window)
  (layout/do-layout)
      (prn (meta #'layout/do-layout-implementation)) ;; TODO: remove me
  )


(defn text2 [string]
  {:string string})

(defn stateful-component [initial-count]
  (let [state-atom (dependable-atom/atom "stateful-component-state"
                                         initial-count)]
    (fn [_initial-count]
      (assoc (text2 (str @state-atom))
             :increase (fn []
                         (swap! state-atom inc))))))

(defn root-component []
  {:children [[stateful-component 1]
              [stateful-component 2]]})

(comment
  (with-bindings (merge (cache/state-bindings)
                        (view-compiler/state-bindings))
    (trace/trace-var #'text2)
    (trace/trace-var #'stateful-component)
    (trace/trace-var #'root-component)

    (let [scene-graph (trace/with-call-tree-printing value-atom
                        (view-compiler/compile-view-calls [root-component]))]
      (pprint/pprint scene-graph)
      ((-> scene-graph :children first :increase)))

    (pprint/pprint (trace/with-call-tree-printing value-atom
                     (view-compiler/compile-view-calls [root-component])))

    )

  )

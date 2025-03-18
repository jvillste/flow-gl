(ns fungl.application-test
  (:require
   [clojure.test :refer [deftest is]]
   [flow-gl.gui.scene-graph :as scene-graph]
   [fungl.application :as application]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layout :as layout]
   [fungl.layouts :as layouts]
   [fungl.util :as util]
   [fungl.view-compiler :as view-compiler]))

(def ^:dynamic log-atom)
(defn log! [type & {:as values}]
  (swap! log-atom conj (merge {:type type}
                              values)))

(defn text [the-text]
  {:type :text
   :string the-text
   :description {:text the-text}
   :draw-function (fn [_gl]
                    (log! :drawing-text :text the-text))
   :width (count the-text)
   :height 1})

(defn counter []
  (let [count-atom (dependable-atom/atom "count" 0)]
    (fn []
      (assoc (text (str @count-atom))
             :keyboard-event-handler (fn [_node event]
                                       (when (and (= :on-target (:phase event))
                                                  (= :key-pressed (:type event)))
                                         (swap! count-atom inc))
                                       event)
             :can-gain-focus? true))))

(defn root-view []
  (layouts/vertically-2 {}
                        [counter]
                        [counter]))

(defn describe [node]
  (merge {:type (:type node)}
         (:description node)))

(defmacro wrap-function [function parameters return-value-symbol prefix-body postfix-body]
  `(let [old-implementation# ~function]
     (fn ~parameters
       ~@prefix-body
       (let [~return-value-symbol (apply old-implementation# ~parameters)]
         ~@postfix-body))))

(defn run-test [root-view
                body-fn]
  (with-redefs [application/process-event! (let [old-process-event! application/process-event!]
                                             (fn [scene-graph event]
                                               (log! :process-event :scene-graph (scene-graph/select-node-keys [:id :compilation-path] scene-graph) :event event)
                                               (old-process-event! scene-graph event)))
                view-compiler/compile-view-calls (let [old-compile-view-calls view-compiler/compile-view-calls]
                                                   (fn [root-view-call]
                                                     (log! :compile-view-calls :root-view-call (util/fully-qualified-function-name-without-random-numbers (first root-view-call)))
                                                     (let [scene-graph (old-compile-view-calls root-view-call)]
                                                       (log! :view-call-compilation-ready :scene-graph (scene-graph/select-node-keys [:id :compilation-path] scene-graph))
                                                       scene-graph)))
                layout/layout-scene-graph (wrap-function layout/layout-scene-graph
                                                         [scene-graph available-width available-height]
                                                         layouted-scene-graph
                                                         []
                                                         [(log! :scene-graph-layout-ready :layouted-scene-graph (layout/select-layout-node-keys [] [:id :compilation-path] layouted-scene-graph))
                                                          layouted-scene-graph])]
    (binding [log-atom (atom [])]
      (with-bindings (application/create-bindings-without-window [root-view])
        (body-fn)
        @log-atom))))

(defn render! []
  (application/render! (fn [width height nodes]
                         {:type :image
                          :nodes nodes
                          :draw-function (fn [gl]
                                           (log! :drawing-image
                                                 :gl gl
                                                 :width width
                                                 :height height
                                                 :nodes (map :type nodes)))})
                       (fn [function]
                         (function :gl))

                       (fn [gl nodes]
                         (log! :rendering-nodes)
                         (doseq [node nodes]
                           ((:draw-function node) gl)))))

(deftest test-1
  (is (= '[{:type :compile-view-calls,
            :root-view-call "fungl.application-test$fn/root-view"}
           {:type :view-call-compilation-ready,
            :scene-graph
            {:id [],
             :compilation-path [:view-call],
             :children
             ({:id [0], :compilation-path [:view-call 0 :view-call]}
              {:id [1], :compilation-path [:view-call 1 :view-call]})}}
           {:type :scene-graph-layout-ready,
            :layouted-scene-graph
            {:node
             {:id [],
              :compilation-path [:view-call],
              :children
              [{:node {:id [0], :compilation-path [:view-call 0 :view-call]}}
               {:node {:id [1], :compilation-path [:view-call 1 :view-call]}}]}}}
           {:type :rendering-nodes}
           {:type :drawing-text, :text "0"}
           {:type :drawing-text, :text "0"}
           {:type :process-event,
            :scene-graph
            {:id [],
             :compilation-path [:view-call],
             :children
             ({:id [0], :compilation-path [:view-call 0 :view-call]}
              {:id [1], :compilation-path [:view-call 1 :view-call]})},
            :event {:source :keyboard, :type :key-pressed}}
           {:type :compile-view-calls,
            :root-view-call "fungl.application-test$fn/root-view"}
           {:type :view-call-compilation-ready,
            :scene-graph
            {:id [],
             :compilation-path [:view-call],
             :children
             ({:id [0], :compilation-path [:view-call 0 :view-call]}
              {:id [1], :compilation-path [:view-call 1 :view-call]})}}
           {:type :scene-graph-layout-ready,
            :layouted-scene-graph
            {:node
             {:id [],
              :compilation-path [:view-call],
              :children
              [{:node {:id [0], :compilation-path [:view-call 0 :view-call]}}
               {:node {:id [1], :compilation-path [:view-call 1 :view-call]}}]}}}
           {:type :rendering-nodes}
           {:type :drawing-text, :text "1"}
           {:type :drawing-image, :gl :gl, :width 1, :height 1, :nodes (:text)}]
         (run-test (fn root-view []
                     (layouts/vertically-2 {}
                                           [counter]
                                           [counter]))
                   (fn test-body []
                     (render!)
                     (application/handle-events! [{:source :keyboard
                                                   :type :key-pressed}])
                     (render!)
                     (render!))))))

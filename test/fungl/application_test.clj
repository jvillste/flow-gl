(ns fungl.application-test
  (:require
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]
   [fungl.swing.root-renderer :as root-renderer]
   [clojure.string :as string]))

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

(defn render-scene-graph [gl scene-graph]
  (log! :render-scene-graph)

  (doseq [node (filter :draw-function
                       (root-renderer/nodes-in-view scene-graph
                                                    (:width scene-graph)
                                                    (:height scene-graph)))]
    (log! :drawing :node (describe node))))

(defn run-test [root-view
                body-fn]
  (with-redefs [application/process-event! (let [old-process-event! application/process-event!]
                                             (fn [scene-graph event]
                                               (log! :process-event :event event)
                                               (old-process-event! scene-graph event)))
                root-renderer/render-to-buffered-image (fn [bounding-box leaf-nodes]
                                                         (log! :renering-to-image)
                                                         (doseq [leaf-node leaf-nodes]
                                                           (log! :drawing :node (describe leaf-node)))
                                                         (merge bounding-box
                                                                {:leaf-nodes leaf-nodes}))
                visuals/image (fn [buffered-image]
                                (merge {:type :image
                                        :description {:nodes (map describe (:leaf-nodes buffered-image))}
                                        :draw-function (fn [_gl]
                                                         (log! :drawing-image
                                                               :nodes (map describe (:leaf-nodes buffered-image))))
                                        :buffered-image buffered-image}
                                       (select-keys buffered-image
                                                    [:x :y :width :height])))]
    (binding [log-atom (atom [])]
      (with-bindings (application/create-bindings-without-window root-view)
        (body-fn)
        @log-atom))))

(defn render! []
  (application/render (:scene-graph @application/state-atom)
                      render-scene-graph
                      nil))

(deftest test-1
  (is (= (run-test (fn []
                     (layouts/vertically-2 {}
                                           [counter]
                                           [counter]))
                   (fn []
                     (render!)
                     (application/handle-events! [{:source :keyboard
                                                   :type :key-pressed}])
                     (render!)
                     (render!)))
         '[{:type :render-scene-graph}
           {:type :drawing, :node {:type :text, :text "0"}}
           {:type :drawing, :node {:type :text, :text "0"}}
           {:type :process-event, :event {:source :keyboard, :type :key-pressed}}
           {:type :renering-to-image}
           {:type :drawing, :node {:type :text, :text "0"}}
           {:type :render-scene-graph}
           {:type :drawing, :node {:type :text, :text "1"}}
           {:type :drawing, :node {:type :image, :nodes ({:type :text, :text "0"})}}])))

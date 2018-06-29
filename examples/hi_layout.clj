(ns examples.hi-layout
  (:require [fungl.application :as application]
            (flow-gl.gui [layout :as layout]
                         [visuals :as visuals])))


(def state (atom {}))

(defn stateful-rectangle [id width]
  (let [rectangle-state (get @state id)]
    (-> (visuals/rectangle (if (:mouse-over rectangle-state)
                             [255 255 255 150]
                             [0 255 255 150])
                           80 80)
        (assoc :width width
               :height (if (:mouse-over rectangle-state)
                         150
                         100)
               :id id
               :mouse-event-handler (fn [node event]
                                      (swap! state update-in [id]
                                             (fn [rectangle-state]
                                               (case (:type event)
                                                 :mouse-entered (assoc rectangle-state :mouse-over true)
                                                 :mouse-left (assoc rectangle-state :mouse-over false)
                                                 rectangle-state)))

                                      event)))))


(def vertical-stack
  {:get-size (fn [node]
               {:width (apply max
                              (conj (map :width (:children node))
                                    0))
                :height (reduce + (map :height (:children node)))})

   :give-space (fn [node]
                 (update-in node [:children]
                            (fn [children]

                              (map (fn [child]
                                     (assoc child
                                            :available-width (:available-width node)
                                            :available-height java.lang.Integer/MAX_VALUE))
                                   children))))
   :make-layout (fn [node]
                  (assoc node :children
                         (loop [layouted-nodes []
                                y 0
                                children (:children node)]
                           (if-let [child (first children)]
                             (recur (conj layouted-nodes
                                          (assoc child
                                                 :x 0
                                                 :y y))
                                    (+ y (:height child))
                                    (rest children))
                             layouted-nodes))))})


(defn create-scene-graph [width height]
  (-> (assoc vertical-stack
             :adapt-to-space (fn [node]
                               (assoc node
                                      :children (loop [index 0
                                                       rectangles []
                                                       y 0]
                                                  (let [rectangle (stateful-rectangle index (:available-width node))
                                                        rectangle-size (layout/size rectangle)]
                                                    (if (< (+ y (:height rectangle-size))
                                                           (:available-height node))
                                                      (recur (inc index)
                                                             (conj rectangles rectangle)
                                                             (+ y (:height rectangle-size)))
                                                      rectangles))))))
      (application/do-layout width height)))

(defn start []
  (application/start-window create-scene-graph)
  #_(.start (Thread. (fn []
                       (start-window)))))

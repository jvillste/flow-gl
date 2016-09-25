(ns flow-gl.gui.mouse
  (:require [clojure.spec :as spec]
            [flow-gl.gui.scene-graph :as scene-graph]
            [clojure.test :as test :refer [deftest is]]))

(spec/def ::mouse-event (spec/keys :req-un [:scene-graph/x :scene-graph/y]))

(defn mouse-event-handler-nodes-in-coodriantes [graph x y]
  (->> (scene-graph/flatten graph)
       
       (filter (fn [node]
                 (and (:width node)
                      (:height node)
                      (scene-graph/in-coordinates? node x y)
                      (contains? node :mouse-event-handler))))
       (sort-by :z)))

(deftest mouse-event-handlers-in-coodriantes-test
  (is (= '({:x 0, :y 5, :width 20, :height 20, :mouse-event-handler 1, :z 0}
           {:x 5, :y 10, :width 5, :height 5, :mouse-event-handler 2, :z 0})
         (mouse-event-handler-nodes-in-coodriantes {:x 0 :y 5 :width 20 :height 20
                                                    :mouse-event-handler 1
                                                    :children [{:x 5 :y 5 :width 5 :height 5
                                                                :mouse-event-handler 2}
                                                               {:x 5 :y 5 :width 5 :height 5}
                                                               {:x 50 :y 5 :width 5 :height 5
                                                                :mouse-event-handler 3}]}
                                                   5 10))))

(defn call-mouse-event-handler-for-node [event node]
  ((:mouse-event-handler node) (assoc event
                                      :local-x (- (:x event)
                                                  (:x node))
                                      :local-y (- (:y event)
                                                  (:y node)))))

(spec/fdef call-mouse-event-handler-for-node
           :args (spec/cat :event ::mouse-event
                           :node ::scene-graph/node))

(defn call-mouse-event-handlers-for-nodes [event nodes]
  (reduce (fn [event node]
            (call-mouse-event-handler-for-node event
                                               node))
          event
          nodes))

(defn call-mouse-event-handlers [graph event]
  (let [handler-nodes (mouse-event-handler-nodes-in-coodriantes graph
                                                                (:x event)
                                                                (:y event))
        parent-nodes (drop-last handler-nodes)
        target-node (last handler-nodes)]
    
    (-> event
        (cond-> (not (empty? parent-nodes))
          (-> (assoc :handling-phase :going-up)
              (call-mouse-event-handlers-for-nodes parent-nodes)))
        (cond-> target-node
          (-> (assoc :handling-phase :on-target)
              (call-mouse-event-handler-for-node target-node)))
        (cond-> (not (empty? parent-nodes))
          (-> (assoc :handling-phase :going-down)
              (call-mouse-event-handlers-for-nodes (reverse parent-nodes)))))))


(spec/fdef call-mouse-event-handlers
           :args (spec/cat :node ::scene-graph/node
                           :event ::mouse-event))



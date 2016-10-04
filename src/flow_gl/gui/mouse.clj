(ns flow-gl.gui.mouse
  (:require [clojure.spec :as spec]
            [clojure.spec.test :as spec-test]
            [clojure.set :as set]
            [flow-gl.gui.scene-graph :as scene-graph]
            [clojure.test :as test :refer [deftest is]]))

(spec/def ::mouse-event (spec/keys :req-un [:scene-graph/x :scene-graph/y]))

(defn mouse-event-handler-nodes-in-coodriantes [graph x y]
  (->> (scene-graph/flatten graph)
       
       (filter (fn [node]
                 (and (:width node)
                      (:height node)
                      (scene-graph/in-coordinates? node x y)
                      (if-let [hit-test (:hit-test node)]
                        (hit-test node
                                  (- x (:x node))
                                  (- y (:y node)))
                        true)
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
  ((:mouse-event-handler node) (if (and (:x event) (:y event))
                                 (assoc event
                                        :local-x (- (:x event)
                                                    (:x node))
                                        :local-y (- (:y event)
                                                    (:y node)))
                                 event)))

(spec/fdef call-mouse-event-handler-for-node
           :args (spec/cat :event ::mouse-event
                           :node ::scene-graph/node))

(defn call-mouse-event-handlers-for-nodes [event nodes]
  (reduce (fn [event node]
            (call-mouse-event-handler-for-node event
                                               node))
          event
          nodes))

(defn call-mouse-event-handlers [nodes-with-mouse-event-handlers event]
  (let [parent-nodes (drop-last nodes-with-mouse-event-handlers)
        target-node (last nodes-with-mouse-event-handlers)]
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
           :args (spec/cat :nodes-with-mouse-event-handlers (spec/coll-of ::scene-graph/node)
                           :event ::mouse-event))


(defn send-mouse-over-events [previous-mouse-event-handlers-by-id current-mouse-event-handler-nodes]
  (let [current-mouse-event-handlers-by-id (reduce (fn [handlers node]
                                                     (if (:id node)
                                                       (assoc handlers (:id node) (:mouse-event-handler node))
                                                       handlers))
                                                   {}
                                                   current-mouse-event-handler-nodes)
        previous-id-set (apply hash-set (keys previous-mouse-event-handlers-by-id))
        current-id-set (apply hash-set (keys current-mouse-event-handlers-by-id))]

    (doseq [new-id (set/difference current-id-set
                                   previous-id-set)]
      ((get current-mouse-event-handlers-by-id
            new-id) 
       {:type :mouse-entered}))
    
    (doseq [old-id (set/difference previous-id-set
                                   current-id-set)]
      ((get previous-mouse-event-handlers-by-id
            old-id) 
       {:type :mouse-left}))
    
    current-mouse-event-handlers-by-id))


(spec/fdef send-mouse-over-events
           :args (spec/cat :previous-mouse-event-handlers-by-id (spec/map-of (constantly true) fn?)
                           :current-mouse-event-handler-nodes (spec/coll-of ::scene-graph/node))
           :ret map?)

(deftest send-mouse-over-events-test
  (spec-test/instrument `send-mouse-over-events)
  (is (= {}
         (send-mouse-over-events {1 (fn [event])}
                                 [])))

  (is (= {:path [:args :previous-mouse-event-handlers-by-id 1],
          :pred 'fn?,
          :val "foo",
          :via [],
          :in [0 1 1]}
         (-> (clojure.test/is (thrown? Exception
                                       (send-mouse-over-events {1 "foo"}
                                                               [])))
             ex-data
             ::spec/problems
             first))))


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
  ((:mouse-event-handler node)
   node
   (if (and (:x event) (:y event))
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

(defn call-handler-for-node [nodes-by-id id event]
  (let [node (get nodes-by-id
                  id)]
    ((:mouse-event-handler node)
     node
     event)))

(defn send-mouse-over-events [previous-mouse-event-handler-nodes-by-id current-mouse-event-handler-nodes]
  (let [current-mouse-event-handler-nodes-by-id (reduce (fn [nodes node]
                                                          (if (and (:id node) (:mouse-event-handler node))
                                                            (assoc nodes (:id node) node)
                                                            nodes))
                                                        {}
                                                        current-mouse-event-handler-nodes)
        previous-id-set (apply hash-set (keys previous-mouse-event-handler-nodes-by-id))
        current-id-set (apply hash-set (keys current-mouse-event-handler-nodes-by-id))]

    (doseq [id (set/difference current-id-set
                               previous-id-set)]
      (call-handler-for-node current-mouse-event-handler-nodes-by-id
                             id
                             {:type :mouse-entered}))
    
    (doseq [id (set/difference previous-id-set
                               current-id-set)]
      (call-handler-for-node previous-mouse-event-handler-nodes-by-id
                             id
                             {:type :mouse-left}))
    
    current-mouse-event-handler-nodes-by-id))


(spec/fdef send-mouse-over-events
           :args (spec/cat :previous-mouse-event-handlers-by-id (spec/map-of (constantly true) ::scene-graph/node)
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

(defn initialize-state []
  {:mouse-event-handler-nodes-under-mouse-by-id {}})



(defn handle-mouse-event [state scene-graph event]
  (let [mouse-event-handler-nodes-under-mouse (mouse-event-handler-nodes-in-coodriantes scene-graph
                                                                                        (:x event)
                                                                                        (:y event))]

    (call-mouse-event-handlers mouse-event-handler-nodes-under-mouse
                               event)

    (update-in state
               [:mouse-event-handler-nodes-under-mouse-by-id]
               send-mouse-over-events
               mouse-event-handler-nodes-under-mouse)))



;; dynamic state

(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom (initialize-state))})

(defn handle-mouse-event! [scene-graph event]
  (swap! state-atom
         handle-mouse-event scene-graph event))

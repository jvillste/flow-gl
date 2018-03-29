(ns flow-gl.gui.mouse
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.test.alpha :as spec-test]
            [clojure.set :as set]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.callable :as callable]
            [clojure.test :as test :refer [deftest is]]))

(spec/def ::mouse-event (spec/keys :req-un [:scene-graph/x :scene-graph/y]))

#_(defn mouse-event-handler-nodes-in-coordinates [graph x y]
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


(defn mouse-event-handler-node-in-coordinates? [x y node]
  (and (:width node)
       (:height node)
       (scene-graph/hits? node x y)
       (contains? node :mouse-event-handler)))

(defn node-in-coordinates? [x y node]
  (scene-graph/hits? node x y))


(defn clip-mouse-event? [x y node]
  (if-let [clip-mouse-events (:clip-mouse-events node)]
    (clip-mouse-events node x y)
    false))

(defn mouse-event-handler-nodes-in-coordinates [node x y]
  (scene-graph/conditionaly-flatten node
                                    (complement (partial clip-mouse-event? x y))
                                    (partial mouse-event-handler-node-in-coordinates? x y)))

(defn nodes-in-coordinates [node x y]
  (scene-graph/conditionaly-flatten node
                                    (complement (partial clip-mouse-event? x y))
                                    (partial node-in-coordinates? x y)))

(deftest mouse-event-handlers-in-coodriantes-test
  (is (= '({:x 0, :y 5, :width 20, :height 20, :mouse-event-handler 1, :z 0}
           {:x 5, :y 10, :width 5, :height 5, :mouse-event-handler 2, :z 0})
         (mouse-event-handler-nodes-in-coordinates {:x 0 :y 5 :width 20 :height 20
                                                    :mouse-event-handler 1
                                                    :children [{:x 5 :y 5 :width 5 :height 5
                                                                :mouse-event-handler 2}
                                                               {:x 5 :y 5 :width 5 :height 5}
                                                               {:x 50 :y 5 :width 5 :height 5
                                                                :mouse-event-handler 3}]}
                                                   5 10)))

  (let [clip (fn [node x y] true)]
    (is (= [{:x 0, :y 5, :width 20, :height 20, :mouse-event-handler 1, :z 0, :clip-mouse-events clip}]
           (mouse-event-handler-nodes-in-coordinates {:x 0 :y 5 :width 20 :height 20
                                                      :mouse-event-handler 1
                                                      :clip-mouse-events clip
                                                      :children [{:x 5 :y 5 :width 5 :height 5
                                                                  :mouse-event-handler 2}
                                                                 {:x 5 :y 5 :width 5 :height 5}
                                                                 {:x 50 :y 5 :width 5 :height 5
                                                                  :mouse-event-handler 3}]}
                                                     5 10)))))

(defn call-mouse-event-handler-for-node [event node]
  (let [event-after-call (callable/call (:mouse-event-handler node)
                                        node
                                        (if (and (:x event) (:y event))
                                          (assoc event
                                                 :local-x (- (:x event)
                                                             (:x node))
                                                 :local-y (- (:y event)
                                                             (:y node)))
                                          event))]
    
    (when (spec/invalid? (spec/conform ::mouse-event event-after-call))
      (throw (ex-info "Handler did not return valid mouse event"
                      {:returned-event event-after-call
                       :node node})))
    
    event-after-call))



(spec/fdef call-mouse-event-handler-for-node
           :args (spec/cat :event ::mouse-event
                           :node ::scene-graph/node))

(defn call-mouse-event-handlers-for-nodes [event nodes]
  (reduce (fn [event node]
            (call-mouse-event-handler-for-node event
                                               node))
          event
          nodes))

#_(defn call-mouse-event-handlers [nodes-with-mouse-event-handlers event]
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

(defn call-mouse-event-handlers-2 [nodes-under-mouse event]
  (let [nodes-with-mouse-event-handlers (filter :mouse-event-handler nodes-under-mouse)
        parent-nodes (drop-last nodes-with-mouse-event-handlers)
        target-node (last nodes-with-mouse-event-handlers)]
    (-> event
        (assoc :nodes-under-mouse nodes-under-mouse)
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



(defn send-mouse-over-events [previous-mouse-event-handler-nodes-by-id current-mouse-event-handler-nodes-by-id]
  (let [previous-id-set (apply hash-set (keys previous-mouse-event-handler-nodes-by-id))
        current-id-set (apply hash-set (keys current-mouse-event-handler-nodes-by-id))]

    (doseq [id (set/difference current-id-set
                               previous-id-set)]
      (call-handler-for-node current-mouse-event-handler-nodes-by-id
                             id
                             {:type :mouse-entered
                              :handling-phase :on-target}))
    
    (doseq [id (set/difference previous-id-set
                               current-id-set)]
      (call-handler-for-node previous-mouse-event-handler-nodes-by-id
                             id
                             {:type :mouse-left
                              :handling-phase :on-target}))
    
    current-mouse-event-handler-nodes-by-id))

(defn send-nodes-under-mouse-changed-events [previous-nodes-under-mouse current-nodes-under-mouse]
  (if (not= previous-nodes-under-mouse current-nodes-under-mouse)
    (let [affected-mouse-event-handler-nodes (reduce (fn [handlers node]
                                                       (if (and (:id node)
                                                                (:mouse-event-handler node))
                                                         (assoc handlers
                                                                (:id node)
                                                                node)
                                                         handlers))
                                                     {}
                                                     (concat previous-nodes-under-mouse
                                                             current-nodes-under-mouse))]

      (doseq [node (vals affected-mouse-event-handler-nodes)]
        (callable/call (:mouse-event-handler node)
         node
         {:type :nodes-under-mouse-changed
          :handling-phase :on-target
          :nodes-under-mouse current-nodes-under-mouse})))))

(spec/fdef send-mouse-over-events
           :args (spec/cat :previous-mouse-event-handlers-by-id (spec/map-of (constantly true) ::scene-graph/node)
                           :current-mouse-event-handler-nodes-by-id (spec/map-of (constantly true) ::scene-graph/node))
           :ret map?)


#_(defn send-uncovered-mouse-over-events [previous-uncovered-mouse-event-handler-node current-uncovered-mouse-event-handler-node]
    (if (not= (:id previous-uncovered-mouse-event-handler-node)
              (:id current-uncovered-mouse-event-handler-node))
      (when previous-uncovered-mouse-event-handler-node
        ((:mouse-event-handler previous-uncovered-mouse-event-handler-node)
         previous-uncovered-mouse-event-handler-node
         {:type :uncovered-mouse-left
          :handling-phase :on-target}))

      (when current-uncovered-mouse-event-handler-node
        ((:mouse-event-handler current-uncovered-mouse-event-handler-node)
         current-uncovered-mouse-event-handler-node
         {:type :uncovered-mouse-entered
          :handling-phase :on-target}))))




(deftest send-mouse-over-events-test
  (spec-test/instrument `send-mouse-over-events)
  
  (is (= {}
         (send-mouse-over-events {1 {:mouse-event-handler (fn [node event])
                                     :x 0
                                     :y 0}}
                                 []))))

(defn initialize-state []
  {:mouse-event-handler-nodes-under-mouse-by-id {}})


(defn nodes-by-id [nodes]
  (reduce (fn [nodes-by-id node]
            (assoc nodes-by-id (:id node) node))
          {}
          nodes))

(defn send-mouse-over-events-and-update-state [state]

  (send-nodes-under-mouse-changed-events (:previous-nodes-under-mouse state)
                                         (:nodes-under-mouse state))

  (assoc state
         :previous-nodes-under-mouse (:nodes-under-mouse state)))

(defn update-nodes-under-mouse [state]
  (assoc state :nodes-under-mouse
         (if (and (:mouse-x state)
                  (:mouse-y state))
           (nodes-in-coordinates (:scene-graph state)
                                 (:mouse-x state)
                                 (:mouse-y state))
           [])))

(defn handle-mouse-event [state event]
  (let [state (if (or (not= (:x event)
                            (:mouse-x event))
                      (not= (:y event)
                            (:mouse-y event)))
                (update-nodes-under-mouse (assoc state
                                                 :mouse-x (:x event)
                                                 :mouse-y (:y event)))
                state)
        #_nodes-under-mouse #_(nodes-in-coordinates (:scene-graph state)
                                                    (:x event)
                                                    (:y event))
        #_uncovered-node-under-mouse #_(last (:nodes-under-mouse state))
        #_mouse-event-handler-nodes-under-mouse #_(filter :mouse-event-handler
                                                          (:nodes-under-mouse state))]

    #_(call-mouse-event-handlers  mouse-event-handler-nodes-under-mouse
                                  (assoc event
                                         :covered (not (:mouse-event-handler uncovered-node-under-mouse))))

    (call-mouse-event-handlers-2 (:nodes-under-mouse state)
                                 event)
    
    (send-mouse-over-events-and-update-state state)))

(defn handle-new-scene-graph [state scene-graph]
  (if (not= scene-graph
            (:scene-graph state))
    (let [state (update-nodes-under-mouse (assoc state
                                                 :scene-graph scene-graph))]

      (send-mouse-over-events-and-update-state state))
    state))



;; dynamic state

(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom (initialize-state))})

(defn handle-mouse-event! [event]
  (swap! state-atom
         handle-mouse-event event))

(defn handle-new-scene-graph! [scene-graph]
  (swap! state-atom
         handle-new-scene-graph scene-graph))

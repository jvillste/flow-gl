(ns flow-gl.gui.mouse
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [clojure.spec.test.alpha :as spec-test]
            [clojure.test :refer [deftest is]]
            [flow-gl.gui.scene-graph :as scene-graph]
            [fungl.callable :as callable]))

(spec/def ::mouse-event (spec/keys :req-un [:scene-graph/x :scene-graph/y]))

(defn nodes-in-coordinates-in-one-containing-root
  ([containing-root target-global-x target-global-y]
   (nodes-in-coordinates-in-one-containing-root containing-root
                                                (if (scene-graph/dimensions-in-coordinates? (:global-x containing-root)
                                                                                            (:global-y containing-root)
                                                                                            (:width containing-root)
                                                                                            (:height containing-root)
                                                                                            target-global-x
                                                                                            target-global-y)
                                                  [containing-root]
                                                  [])
                                                0
                                                0
                                                target-global-x
                                                target-global-y))

  ([node nodes-in-coordinates parent-x parent-y target-global-x target-global-y]
   (let [node-global-x (+ parent-x (or (:x node)
                                       0))
         node-global-y (+ parent-y (or (:y node)
                                       0))]
     (if (:children node)
       (loop [nodes-in-coordinates nodes-in-coordinates
              contained-children (filter (fn [child]
                                           (scene-graph/parent-contains-child? node child))
                                         (:children node))]
         (if-let [contained-child (first contained-children)]
           (if (scene-graph/dimensions-in-coordinates? (+ node-global-x
                                                          (:x contained-child))
                                                       (+ node-global-y
                                                          (:y contained-child))
                                                       (:width contained-child)
                                                       (:height contained-child)
                                                       target-global-x
                                                       target-global-y)
             (recur (nodes-in-coordinates-in-one-containing-root contained-child
                                                                 (conj nodes-in-coordinates
                                                                       contained-child)
                                                                 node-global-x
                                                                 node-global-y
                                                                 target-global-x
                                                                 target-global-y)
                    (rest contained-children))

             (recur nodes-in-coordinates
                    (rest contained-children)))

           nodes-in-coordinates))
       nodes-in-coordinates))))

(deftest test-nodes-in-coordinates-in-one-containing-root
  (is (= []
         (nodes-in-coordinates-in-one-containing-root {:x 10 :y 0 :width 100 :height 100 :global-x 10 :global-y 0}
                                                      0
                                                      0)))

  (is (= [{:x 10, :y 0, :width 100, :height 100, :global-x 10, :global-y 0}]
         (nodes-in-coordinates-in-one-containing-root {:x 10 :y 0 :width 100 :height 100 :global-x 10 :global-y 0}
                                                      10
                                                      0)))

  (is (= [{:x 10,
           :y 0,
           :width 100,
           :height 100,
           :global-x 10,
           :global-y 0,
           :children
           [{:x 5,
             :y 0,
             :width 1000,
             :height 100,
             :children [{:x 10, :y 10, :width 100, :height 1000}]}
            {:x 0, :y 0, :width 100, :height 100}]}
          {:x 0, :y 0, :width 100, :height 100}]
         (nodes-in-coordinates-in-one-containing-root {:x 10 :y 0 :width 100 :height 100 :global-x 10, :global-y 0
                                                       :children [{:x 5 :y 0 :width 1000 :height 100
                                                                   :children [{:x 10 :y 10 :width 100 :height 1000}]}
                                                                  {:x 0 :y 0 :width 100 :height 100}]}
                                                      20
                                                      0))))


(defn nodes-in-coordinates [containing-roots global-x global-y]
  (apply concat
         (map (fn [containing-root]
                (nodes-in-coordinates-in-one-containing-root containing-root
                                                             global-x
                                                             global-y))
              containing-roots)))



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
  (assoc state
         :nodes-under-mouse
         (if (and (:mouse-x state)
                  (:mouse-y state))
           (nodes-in-coordinates (:containing-roots state)
                                 (:mouse-x state)
                                 (:mouse-y state))
           [])))

(defn handle-mouse-event [state event]
  (let [state (if (and (= (:x event)
                          (:mouse-x event))
                       (= (:y event)
                          (:mouse-y event)))
                state
                (-> state
                    (assoc :mouse-x (:x event)
                           :mouse-y (:y event))
                    (update-nodes-under-mouse)))
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
  (if (= scene-graph
         (:scene-graph state))
    state
    (-> state
        (assoc :containing-roots (scene-graph/containing-roots scene-graph)
               :scene-graph scene-graph)
        (update-nodes-under-mouse)
        (send-mouse-over-events-and-update-state))))



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

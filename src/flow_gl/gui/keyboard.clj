(ns flow-gl.gui.keyboard
  (:require [clojure.test :refer [deftest is]]
            [flow-gl.gui.scene-graph :as scene-graph]
            [flow-gl.utils :as utils]
            [fungl.cache :as cache]
            [fungl.callable :as callable]
            [medley.core :as medley]
            [fungl.layout :as layout]))

(defn- initialize-state []
  {})

(defn- call-handler [handler & arguments]
  (apply callable/call handler arguments))

(defn- call-if-set [keyboard-state handler-key & arguments]
  (when-let [handler (handler-key keyboard-state)]
    (apply call-handler handler arguments)))

(defn set-focused-event-handler [keyboard-state event-handler]
  (assert (or (nil? event-handler)
              (map? keyboard-state)))

  (assoc keyboard-state
         :focused-handler event-handler))


(defn- call-focused-event-handler [state event]
  (call-if-set state :focused-handler event))


;; focused nodes

(defn set-focused-node
  ([keyboard-state node]

   (assert (or (nil? node)
               (:id node))
           "Focused node must have :id")

   (assert (or (nil? node)
               (:keyboard-event-handler node))
           "Focused node must have :keyboard-event-handler")

   (-> keyboard-state
       (assoc :focused-node-id (:id node))
       (set-focused-event-handler (:keyboard-event-handler node)))))


;; focus cycling

(defn- filter-keyboard-event-handler-nodes [nodes]
  (filter :keyboard-event-handler nodes))

(defn order-nodes-down-right [nodes]
  (sort-by (fn [node] [(:y node) (:x node)])
           nodes))

(defn- node-position [nodes id]
  (-> (utils/positions #(= (:id %) id)
                       nodes)
      (first)))

(defn cycle-position [min max position]
  (if (> position max)
    min
    (if (< position min)
      max
      position)))

(defn- get-with-limit [position value-vector limit]
  (get value-vector
       (limit 0
              (dec (count value-vector))
              position)))

(defn keyboard-event-handler-nodes-from-scene-graph [scene-graph]
  (-> scene-graph
      (scene-graph/flatten)
      (filter-keyboard-event-handler-nodes)))

(defn- next-node [currently-focused-node-id nodes ordering advance limit]
  (let [nodes-with-handler (-> nodes
                               (ordering)
                               (vec))]

    (-> (node-position nodes-with-handler
                       (or currently-focused-node-id
                           (:id (first nodes-with-handler))))
        (advance)
        (get-with-limit nodes-with-handler
                        limit))))

(defn- move-focus [state scene-graph ordering advance limit]
  (set-focused-node state
                    (next-node (:focused-node-id state)
                               (filter :can-gain-focus?
                                       (keyboard-event-handler-nodes-from-scene-graph scene-graph))
                               ordering
                               advance
                               limit)))



;; state

(defn move-focus-and-send-move-events! [state-atom move-focus & arguments]
  (let [old-handler-atom (atom nil)
        old-node-id-atom (atom nil)]
    (swap! state-atom
           (fn [keyboard-state]
             (reset! old-handler-atom (:focused-handler keyboard-state))
             (reset! old-node-id-atom (:focused-node-id keyboard-state))
             (apply move-focus
                    keyboard-state
                    arguments)))
    (let [state @state-atom]
      (when (not (= (:focused-handler state)
                    @old-handler-atom))
        (when @old-handler-atom
          (call-handler @old-handler-atom
                        (if @old-node-id-atom
                          (scene-graph/find-first #(= @old-node-id-atom (:id %))
                                                  (:scene-graph state))
                          (:scene-graph state))
                        {:type :focus-lost}))

        (call-handler (:focused-handler state)
                      (if (:focused-node-id state)
                        (scene-graph/find-first #(= (:focused-node-id state) (:id %))
                                                (:scene-graph state))
                        (:scene-graph state))
                      {:type :focus-gained})))))

(defn focused-node-id [state]
  (:focused-node-id state))

(defn keyboard-event-handler-nodes [state]
  (keyboard-event-handler-nodes-from-scene-graph (:scene-graph state)))

(defn focused-node [state]
  (first (filter (fn [node]
                   (= (:id node)
                      (focused-node-id state)))
                 (keyboard-event-handler-nodes state))))

;; dynamic state

(def ^:dynamic state-atom)

(defn state-bindings []
  {#'state-atom (atom (initialize-state))})


(defn set-focused-event-handler! [event-handler]
  (when (not= event-handler
              (:focused-handler @state-atom))
    (move-focus-and-send-move-events! state-atom
                                      set-focused-event-handler
                                      event-handler)))

(defn call-focused-event-handler! [event]
  (let [state @state-atom]
    (when-let [focused-handler (:focused-handler state)]
      (if-let [focused-node-id (:focused-node-id state)]
        (call-handler focused-handler
                      (scene-graph/find-first #(= focused-node-id (:id %))
                                              (:scene-graph state))
                      event)
        (call-handler focused-handler
                      (:scene-graph state)
                      event)))))

(defn set-focused-node! [node]
  (move-focus-and-send-move-events! state-atom
                                    set-focused-node
                                    node))

(defn update-nodes-event-handler! [node event-handler]

  (when (= (:id node)
           (:focused-node-id @state-atom))
    #_(flow-gl.tools.trace/log "setting focused handler" (:focused-node-id @state-atom) event-handler)
    (swap! state-atom set-focused-event-handler event-handler))

  (assoc node :keyboard-event-handler event-handler))


(defn keyboard-event-handlers
  ([scene-graph]
   (cache/call! keyboard-event-handlers scene-graph {}))
  ([scene-graph handlers]
   (let [handlers (if-let [keyboard-event-handler (:keyboard-event-handler scene-graph)]
                    (conj {(:id scene-graph)
                           keyboard-event-handler}
                          handlers)
                    handlers)]
     (loop [children (:children scene-graph)
            handlers handlers]
       (if-let [child (first children)]
         (recur (rest children)
                (conj handlers
                      (cache/call! keyboard-event-handlers child)))
         handlers)))))

(deftest keyboard-event-handlers-test
  (is (= {1 :handler-1}
         (keyboard-event-handlers {:id 1
                                   :keyboard-event-handler :handler-1})))

  (is (= {1 :handler-1
          2 :handler-2}
         (keyboard-event-handlers {:id 1
                                   :keyboard-event-handler :handler-1
                                   :children [{:id 2
                                               :keyboard-event-handler :handler-2}]})))

  (with-bindings (cache/state-bindings)
    (is (= {1 :handler-1
            2 :handler-2
            5 :handler-5}
           (keyboard-event-handlers {:id 1
                                     :keyboard-event-handler :handler-1
                                     :children [{:id 2
                                                 :keyboard-event-handler :handler-2}
                                                {:id 3}
                                                {:id 4
                                                 :children [{:id 5
                                                             :keyboard-event-handler :handler-5}]}]})))))

(defn handle-new-scene-graph! [scene-graph]

  (set-focused-node! (or (scene-graph/find-first (fn [node]
                                                   (= (:focused-node-id @state-atom)
                                                      (:id node)))
                                                 scene-graph)
                         (scene-graph/find-first :can-gain-focus?
                                                 scene-graph)))

  (swap! state-atom assoc :scene-graph scene-graph))

(defn set-focus-on-mouse-clicked! [node event]
  (when (and (= :mouse-clicked
                (:type event))
             (not= (:id node)
                   (:focused-node-id @state-atom)))
    (set-focused-node! node))
  event)


(defn move-focus! [scene-graph ordering advance limit]
  (move-focus-and-send-move-events! state-atom
                                    move-focus
                                    scene-graph ordering advance limit))


(defn cycle-focus [scene-graph event]
  (move-focus! scene-graph
               order-nodes-down-right
               (if (:shift event)
                 dec
                 inc)
               cycle-position))

(defn propagate-event! [path event]
  (let [descended-event (loop [event event
                               path path]
                          (if-let [node (first path)]
                            (let [returned-event (if (:keyboard-event-handler node)
                                                   (call-handler (:keyboard-event-handler node)
                                                                 node
                                                                 (assoc event
                                                                        :phase (if (empty? (rest path))

                                                                                 :on-target
                                                                                 :descent)))
                                                   event)]
                              (when returned-event
                                (recur returned-event
                                       (rest path))))

                            event))]
    (when descended-event
      (loop [event (assoc descended-event :phase :ascent)
             path (rest (reverse path))]
        (when-let [node (first path)]
          (let [returned-event (if (:keyboard-event-handler node)
                                 (call-handler (:keyboard-event-handler node)
                                               node
                                               event)
                                 event)]
            (when returned-event
              (recur returned-event
                     (rest path)))))))))

(defn handle-keyboard-event! [_scene-graph event]
  (let [state @state-atom]
    (if-let [focused-node-id (:focused-node-id state)]
      (propagate-event! (scene-graph/path-to-first #(= focused-node-id (:id %))
                                                   (:scene-graph state))
                        event)
      (call-focused-event-handler! event))))


;; events

(defn key-pressed? [keyboard-event key]
  (and (= (:key keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

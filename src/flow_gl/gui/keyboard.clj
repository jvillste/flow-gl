(ns flow-gl.gui.keyboard
  (:require [clojure.spec.alpha :as spec]
            [fungl.cache :as cache]
            [flow-gl.utils :as utils]
            (fungl [callable :as callable])
            [taoensso.timbre :as timbre]
            (flow-gl.gui [scene-graph :as scene-graph])
            [clojure.test :as test :refer [deftest is]]))

(defn initialize-state []
  {})

(defn call-handler [handler & arguments]
  (apply callable/call handler arguments))

(defn call-if-set [keyboard-state handler-key & arguments]
  (when-let [handler (handler-key keyboard-state)]
    (apply call-handler handler arguments)))

(defn send-focus-move-events [old-event-handler new-event-handler]
  (when old-event-handler
    (call-handler old-event-handler {:type :focus-lost}))
  
  (call-handler new-event-handler {:type :focus-gained}))

(defn set-focused-event-handler [keyboard-state event-handler]
  (assert (map? keyboard-state))
  
  (assoc keyboard-state
         :focused-handler event-handler))


(defn call-focused-event-handler [state event]
  (call-if-set state :focused-handler event))


;; focused nodes

(defn set-focused-node
  ([keyboard-state node]
   (set-focused-node keyboard-state
                     (:id node)
                     (:keyboard-event-handler node)))
  
  ([keyboard-state node-id event-handler]
   (-> keyboard-state
       (assoc :focused-node-id node-id)
       (set-focused-event-handler event-handler))))




;; focus cycling

(defn filter-keyboard-event-handler-nodes [nodes]
  (filter :keyboard-event-handler nodes))

(defn order-nodes-down-right [nodes]
  (sort-by (fn [node] [(:y node) (:x node)])
           nodes))

(defn node-position [nodes id]
  (-> (utils/positions #(= (:id %) id)
                       nodes)
      (first)))

(defn cycle-position [min max position]
  (if (> position max)
    min
    (if (< position min)
      max
      position)))

(defn get-with-limit [position value-vector limit]
  (get value-vector
       (limit 0
              (dec (count value-vector))
              position)))

(defn keyboard-event-handler-nodes [scene-graph]
  (-> scene-graph
      (scene-graph/flatten)
      (filter-keyboard-event-handler-nodes)))

(defn next-node [currently-focused-node-id nodes ordering advance limit]
  (let [nodes-with-handler (-> nodes
                               (ordering)
                               (vec))]

    (-> (node-position nodes-with-handler
                       (or currently-focused-node-id
                           (:id (first nodes-with-handler))))
        (advance)
        (get-with-limit nodes-with-handler
                        limit))))

(defn move-focus [state scene-graph ordering advance limit]
  (set-focused-node state
                    (next-node (:focused-node-id state)
                               (keyboard-event-handler-nodes scene-graph)
                               ordering
                               advance
                               limit)))


;; state

(defn move-focus-and-send-move-events! [state-atom move-focus & arguments]
  (let [old-handler (atom nil)]
    (swap! state-atom
           (fn [keyboard-state]
             (reset! old-handler (:focused-handler keyboard-state))
             (apply move-focus
                    keyboard-state
                    arguments)))
    (send-focus-move-events @old-handler
                            (:focused-handler @state-atom))))

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
  (call-if-set @state-atom :focused-handler event))



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
  (when-let [focused-handler (get (keyboard-event-handlers scene-graph)
                                  (:focused-node-id @state-atom))]
    (set-focused-event-handler! focused-handler)))

(defn set-focus-on-mouse-clicked! [node event]
  (when (and (= :mouse-clicked
                (:type event))
             (not= (:id node)
                   (:focused-node-id @state-atom)))
    #_(swap! state-atom set-focused-node (:id node) (:keyboard-event-handler node))
    (set-focused-node! node))
  event)


(defn move-focus! [scene-graph ordering advance limit]
  (move-focus-and-send-move-events! state-atom
                                    move-focus
                                    scene-graph ordering advance limit))


(defn handle-keyboard-event! [scene-graph keyboard-event]
  (if (and (= (:key keyboard-event)
              :tab)
           (= (:type keyboard-event)
              :key-pressed))

    (move-focus! scene-graph
                 order-nodes-down-right
                 (if (:shift keyboard-event)
                   dec
                   inc)
                 cycle-position)
    
    (call-focused-event-handler! keyboard-event)))

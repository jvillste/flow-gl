(ns flow-gl.gui.keyboard
  (:require [clojure.spec :as spec]
            [flow-gl.utils :as utils]
            (flow-gl.gui [scene-graph :as scene-graph])
            [clojure.test :as test :refer [deftest is]]))


(def ^:dynamic keyboard-state-atom)

(defn initialize-keybaord-state []
  (atom {}))

(defmacro with-keyboard-state [keyboard-state & body]
  `(binding [keyboard-state-atom ~keyboard-state]
     ~@body))

(defn call-if-set [keyboard-state handler-key & arguments]
  (when-let [handler (handler-key keyboard-state)]
    (apply handler arguments)))

(defn send-focus-move-events! [old-event-handler new-event-handler]
  (when old-event-handler
    (old-event-handler {:type :focus-lost}))
  
  (new-event-handler {:type :focus-gained}))

(defn move-focus-and-send-move-events! [move-focus & arguments]
  (let [old-handler (atom nil)]
    (swap! keyboard-state-atom
           (fn [keyboard-state]
             (reset! old-handler (:focused-handler keyboard-state))
             (apply move-focus
                    keyboard-state
                    arguments)))
    (send-focus-move-events! @old-handler
                             (:focused-handler @keyboard-state-atom))))

(defn set-focused-event-handler [keyboard-state event-handler]
  (assoc keyboard-state
         :focused-handler event-handler))


(defn set-focused-event-handler! [event-handler]
  (move-focus-and-send-move-events! set-focused-event-handler event-handler))

(defn call-focused-event-handler [event]
  (call-if-set @keyboard-state-atom :focused-handler event))


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

(defn set-focused-node! [node]
  (move-focus-and-send-move-events! set-focused-node
                                    node))

;; focus by mouse

(defn set-focus-on-mouse-clicked! [node event]
  (when (= :mouse-clicked
           (:type event))
    (set-focused-node! node))
  event)


;; focus cycling

(defn filter-keyboard-event-handler-nodes [nodes]
  (filter :keyboard-event-handler nodes))

(defn order-nodes-down-right [nodes]
  (sort-by (fn [node] [(:y node) (:x node)])
           nodes))

(defn node-position [nodes id]
  (prn id)
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
                       currently-focused-node-id)
        (advance)
        (get-with-limit nodes-with-handler
                        limit))))

(defn move-focus! [scene-graph ordering advance limit]
  (move-focus-and-send-move-events! (fn [keyboard-state]
                                      (prn keyboard-state)
                                      (set-focused-node keyboard-state
                                                        (next-node (:focused-node-id keyboard-state)
                                                                   (keyboard-event-handler-nodes scene-graph)
                                                                   ordering
                                                                   advance
                                                                   limit)))))



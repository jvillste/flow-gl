(ns flow-gl.gui.keyboard
  (:require [clojure.test :refer [deftest is]]
            [flow-gl.gui.scene-graph :as scene-graph]
            [flow-gl.utils :as utils]
            [fungl.cache :as cache]
            [fungl.callable :as callable]
            [medley.core :as medley]
            [fungl.layout :as layout]
            [fungl.dependable-atom :as dependable-atom]
            [fungl.view-compiler :as view-compiler]
            [fungl.derivation :as derivation]
            [fungl.util :as util]))

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

(defn- set-focused-node [keyboard-state node]
  (assert (or (nil? node)
              (:id node))
          "Focused node must have :id")

  (-> keyboard-state
      (assoc :focused-node-id (:id node)
             :focused-node node)
      (set-focused-event-handler (:keyboard-event-handler node))))


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
                                       (scene-graph/flatten scene-graph))
                               ordering
                               advance
                               limit)))


(defn keyboard-event? [value]
  (and (map? value)
       (= :keyboard (:source value))))

;; state

(defn choose-returned-event [returned-value event]
  (cond (keyboard-event? returned-value)
        returned-value

        (= :stop-propagation
           returned-value)
        nil

        :else
        event))

(defn propagate-event! [path event]
  (let [descended-event (loop [event event
                               path path]
                          (if-let [node (first path)]
                            (let [returned-event (if (:keyboard-event-handler node)
                                                   (choose-returned-event (call-handler (:keyboard-event-handler node)
                                                                                        node
                                                                                        (assoc event
                                                                                               :phase (if (empty? (rest path))

                                                                                                        :on-target
                                                                                                        :descent)
                                                                                               :target-depth (dec (count path))))
                                                                          event)

                                                   event)]
                              (when returned-event
                                (recur returned-event
                                       (rest path))))

                            event))]
    (when descended-event
      (loop [event (assoc descended-event :phase :ascent)
             path (rest (reverse path))
             target-depth 1]
        (when-let [node (first path)]
          (let [returned-event (if (:keyboard-event-handler node)
                                 (choose-returned-event (call-handler (:keyboard-event-handler node)
                                                                      node
                                                                      (assoc event :target-depth target-depth))

                                                        event)
                                 event)]
            (when returned-event
              (recur returned-event
                     (rest path)
                     (inc target-depth)))))))))

(defn move-focus-and-send-move-events! [state-atom move-focus & arguments]
  (let [[old-state new-state] (swap-vals! state-atom
                                          (fn [keyboard-state]
                                            (apply move-focus
                                                   keyboard-state
                                                   arguments)))]
    (when (not (= (-> new-state :focused-node :id)
                  (-> old-state :focused-node :id)))

      (if (:focused-node-id old-state)
        (propagate-event! (scene-graph/path-to-first #(= (:focused-node-id old-state) (:id %))
                                                     (:scene-graph new-state))
                          {:type :focus-lost})
        (when (:focused-handler old-state)
          (call-handler (:focused-handler old-state)
                        (:scene-graph new-state)
                        {:type :focus-lost})))

      (if (:focused-node-id new-state)
        (propagate-event! (scene-graph/path-to-first #(= (:focused-node-id new-state) (:id %))
                                                     (:scene-graph new-state))
                          {:type :focus-gained})
        (when (:focused-handler new-state)
          (call-handler (:focused-handler new-state)
                        (:scene-graph new-state)
                        {:type :focus-gained}))))))

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

(derivation/def-derivation focused-node
  (:focused-node @state-atom))

(derivation/def-derivation focused-node-id-derivation
  (:focused-node-id @state-atom))

(defn state-bindings []
  {#'state-atom (dependable-atom/atom "keyboard-state"
                                      (initialize-state))})

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

(defn handle-next-scene-graph! [handler]
  (swap! state-atom update :next-scene-graph-handlers (fnil conj []) handler))

(defn handle-new-scene-graph! [scene-graph]
  (swap! state-atom assoc :scene-graph scene-graph)

  (doseq [handler (:next-scene-graph-handlers @state-atom)]
    (handler scene-graph))

  (swap! state-atom assoc :next-scene-graph-handlers [])

  (if-let [focused-node-id (:focused-node-id @state-atom)]
    (let [focused-path (->> (scene-graph/id-to-local-id-path focused-node-id)
                            (scene-graph/nodes-on-local-id-path scene-graph)
                            (reverse)
                            (doall))]
      (when (not (= focused-node-id
                    (:id (first focused-path))))
        (when-let [first-focusable-node (or (scene-graph/find-first-child :can-gain-focus?
                                                                          (first focused-path))
                                            (medley/find-first :can-gain-focus?
                                                               focused-path)

                                            (scene-graph/find-first :can-gain-focus? scene-graph))]
          (set-focused-node! first-focusable-node))))
    (when-let [focusable-node (scene-graph/find-first :can-gain-focus? scene-graph)]
      (set-focused-node! focusable-node))))

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

(defn move-focus-2! [node]
  (move-focus-and-send-move-events! state-atom
                                    set-focused-node
                                    node))


(defn cycle-focus [scene-graph & [forward?]]
  (move-focus! scene-graph
               order-nodes-down-right
               (if (if (some? forward?)
                     forward?
                     true)
                 inc
                 dec)
               cycle-position))

(defn handle-keyboard-event! [_scene-graph event]
  (let [state @state-atom]
    (if-let [focused-node-id (:focused-node-id state)]
      (propagate-event! (scene-graph/path-to-first #(= focused-node-id (:id %))
                                                   (:scene-graph state))
                        event)
      (call-focused-event-handler! event))))

(defn sub-component-is-in-focus? []
  (= view-compiler/id
     (take (count view-compiler/id)
           @focused-node-id-derivation
           #_(:focused-node-id @state-atom))))

(defn component-is-in-focus? []
  (= view-compiler/id
     @focused-node-id-derivation
     #_(:focused-node-id @state-atom)))

;; events

(defn key-pressed? [keyboard-event key]
  (and (= (:key keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn event-to-key-pattern [event]
  (when (= (:source event)
           :keyboard)
    [(into #{}
           (remove nil?)
           [(when (:control? event)
              :control)
            (when (:shift? event)
              :shift)
            (when (:alt? event)
              :alt)
            (when (:meta? event)
              :meta)])
     (:key event)]))

(deftest test-event-to-key-pattern
  (is (= [#{:shift} :n]
         (event-to-key-pattern {:key-code 78
                                :alt? false
                                :key :n
                                :control? false
                                :time 1646884415009
                                :phase :descent
                                :type :key-pressed
                                :source :keyboard
                                :shift? true
                                :is-auto-repeat nil
                                :character \n}))))

(defn key-pattern-pressed? [key-pattern event]
  (and (= :key-pressed (:type event))
       (= key-pattern
          (event-to-key-pattern event))))

(defn key-patterns-match? [triggered-key-patterns command-key-patterns]
  (if (vector? (first (first command-key-patterns)))
    (some (fn [command-key-patterns]
            (= triggered-key-patterns
               command-key-patterns))
          command-key-patterns)
    (= triggered-key-patterns
       command-key-patterns)))

(defn key-patterns-prefix-match? [triggered-key-patterns command-key-patterns]
  (if (vector? (first (first command-key-patterns)))
    (some (fn [command-key-patterns]
            (util/starts-with? triggered-key-patterns
                               command-key-patterns))
          command-key-patterns)
    (util/starts-with? triggered-key-patterns
                       command-key-patterns)))

(deftest test-key-patterns-prefix-match?
  (is (= true
         (key-patterns-prefix-match? [[#{:meta} :a]]
                                     [[#{:meta} :a] [#{:meta} :b]])))

  (is (= false
         (key-patterns-prefix-match? [[#{:meta} :a]]
                                     [[#{:meta} :c] [#{:meta} :b]])))

  (is (= true
         (key-patterns-prefix-match? [[#{:meta} :a]]
                                     [[[#{:meta} :c] [#{:meta} :b]]
                                      [[#{:meta} :a] [#{:meta} :b]]])))
  (is (= nil
         (key-patterns-prefix-match? [[#{:meta} :a]]
                                     [[[#{:meta} :c] [#{:meta} :b]]
                                      [[#{:meta} :d] [#{:meta} :b]]]))))

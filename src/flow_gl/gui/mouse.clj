(ns flow-gl.gui.mouse
  (:require (flow-gl.gui [layout :as layout])
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.dataflow :as dataflow]
            [flow-gl.dataflow.triple-dataflow :as triple-dataflow])
  (:use clojure.test
        flow-gl.threading))


;; MOUSE

(defn invert-mouse-y [window-height mouse-event]
  (if (contains? mouse-event :mouse-y)
    (assoc mouse-event :mouse-y (- window-height
                                   (:mouse-y mouse-event)))
    mouse-event))



(defn call-mouse-event-handlers [view-state event mouse-event-handlers]
  (reduce (fn [view-state mouse-event-handler]
            ((:handler mouse-event-handler) view-state event))
          view-state
          mouse-event-handlers))


(defn layoutables-in-coordinates [view-state x y]
  (let [layout (get view-state [:layout])]
    (concat [layout]
            (layout/children-in-coordinates layout
                                            view-state
                                            x
                                            y))))

(defn mouse-event-handlers-in-coordinates [view-state x y]
  (->> (debug/debug :default "layoutables-in-coordinates: " (vec (layoutables-in-coordinates view-state x y)))
       (filter #(not (= nil
                        (:mouse-event-handler %))))
       (map :mouse-event-handler)))

(defn call-mouse-enter-and-leave-handlers [view-state current-mouse-event-handlers-under-mouse old-mouse-event-handlers-under-mouse]
  (let [current-set (set (map :id current-mouse-event-handlers-under-mouse))
        old-set (set (map :id old-mouse-event-handlers-under-mouse))

        mouse-left (filter #(not (contains? current-set (:id %)))
                           old-mouse-event-handlers-under-mouse)

        mouse-entered (filter #(not (contains? old-set (:id %)))
                              current-mouse-event-handlers-under-mouse)]
    (-> view-state
        (call-mouse-event-handlers {:type :mouse-entered} mouse-entered)
        (call-mouse-event-handlers {:type :mouse-left} mouse-left))))


(deftest call-mouse-enter-and-leave-handlers-test
  (let [create-handler (fn [id]
                         (fn [state event]
                           (conj state [id event])))]
    (is (= (call-mouse-enter-and-leave-handlers []
                                                [{:id 1 :handler (create-handler 1)}]

                                                [{:id 1 :handler (create-handler 2)}])
           []))))

(defn update-mouse-event-handlers-under-mouse [view-state]
  (let [current-mouse-event-handlers-under-mouse (mouse-event-handlers-in-coordinates view-state
                                                                                      (:mouse-x view-state)
                                                                                      (:mouse-y view-state))]

    (flow-gl.debug/debug :events "current-mouse-event-handlers-under-mouse " (vec current-mouse-event-handlers-under-mouse))

    (-> view-state
        (call-mouse-enter-and-leave-handlers current-mouse-event-handlers-under-mouse
                                             (:mouse-event-handlers-under-mouse view-state))
        (assoc :mouse-event-handlers-under-mouse current-mouse-event-handlers-under-mouse))))

(defn update-mouse-position [view-state event]
  (-> view-state
      (assoc :mouse-x (:mouse-x event)
             :mouse-y (:mouse-y event))
      (update-mouse-event-handlers-under-mouse)))

(defn capture-mouse [view-state capturer]
  (assoc view-state :mouse-capturer capturer))

(defn release-mouse [view-state]
  (dissoc view-state :mouse-capturer))

(defn send-mouse-event [view-state event]
  (let [handlers (if-let [mouse-capturer (:mouse-capturer view-state)]
                   [{:handler mouse-capturer}]
                   (:mouse-event-handlers-under-mouse view-state))]
    (-> view-state

        (call-mouse-event-handlers (assoc event :event-handling-direction :down)
                                   handlers)

        #_(call-mouse-event-handlers (assoc event :event-handling-direction :up)
                                     (reverse handlers)))))

#_(defn add-mouse-event-handler [layoutable id new-handler]
    (assoc layoutable
      :mouse-event-handler {:id (dataflow/absolute-path id)
                            :handler (fn [view-state event]
                                       (if-let [handler (:mouse-event-handler layoutable)]
                                         (-> ((:handler handler) view-state event)
                                             (new-handler event))
                                         (new-handler view-state event)))}))

(defn handle-mouse-event [view-state event]
  (-> view-state
      (when-> (= (:type event) :mouse-moved)
              (update-mouse-position event))
      (send-mouse-event event)))



(defn trim-mouse-movements [events]
  (letfn [(is-mouse-move [event] (= :mouse-moved (:type event)))]
    (loop [events events
           trimmed-events []]
      (if-let [event (first events)]
        (recur (rest events)
               (if (and (is-mouse-move event)
                        (is-mouse-move (first (rest events))))
                 trimmed-events
                 (conj trimmed-events event)))
        trimmed-events))))


#_(defn with-mouse-over [layoutable key]
  (let [this (dataflow/absolute-path [])]
    (dataflow/initialize key false)
    (add-mouse-event-handler layoutable [this key]
                             (fn [application-state event]
                               (case (:type event)
                                 :mouse-entered (dataflow/define-property-to application-state this key true)
                                 :mouse-left (dataflow/define-property-to application-state this key false)
                                 application-state)))))

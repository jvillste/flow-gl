(ns fungl.value-registry
  (:require [clojure.set :as set])
  (:refer-clojure :exclude [get]))

(defn initialize-state [& {:keys [delete-after-gets] :or {delete-after-gets nil}}]
  {:delete-after-gets delete-after-gets
   :referenced #{}
   :references-after-delete 0})

(defn initialize-value [state id specification]
  (-> state
      (assoc-in [:values id] ((:create specification)))
      (assoc-in [:specifications id] specification)))

(defn get-value [state id]
  (get-in state [:values id]))

(defn ensure-value [state id specification]
  (if (get-in state [:values id])
    state
    (initialize-value state id specification)))

(defn remove-value [state id]
  (-> state
      (update-in [:values] dissoc id)
      (update-in [:specifications] dissoc id)))

(defn unused-ids [state]
  (set/difference (apply hash-set (keys (:values state)))
                  (:referenced state)))


(defn remove-unused-values [state]
  (-> (reduce remove-value
              state
              (unused-ids state))
      (assoc :referenced #{})
      (assoc :references-after-delete 0)))

(defn mark-reference [state id]
  (-> state
      (update-in [:referenced] conj id)
      (update-in [:references-after-delete] inc)))



;; here be side effects

(defn get [state-atom id specification]
  (let [state (swap! state-atom
                     (fn [state]
                       (-> state
                           (ensure-value id specification)
                           (mark-reference id))))
        value (get-value state id)]

    (when-let [on-get (get-in @state-atom [:specifications id :on-get])]
      (on-get id
              value))

    value))

(defn swap-and-return-old-and-new! [atom f & args]
  (loop []
    (let [old @atom
          new (apply f old args)]
      (if (compare-and-set! atom old new)
        [old new]
        (recur)))))

(defn delete-unused-values [state-atom minimum-references-after-previous-delete]
  (when (> (:references-after-delete @state-atom)
           minimum-references-after-previous-delete)
    (let [[old-state new-state] (swap-and-return-old-and-new! state-atom remove-unused-values)]
      (doseq [id (unused-ids old-state)]
        (when-let [destructor (get-in old-state [:specifications id :on-delete])]
          (destructor (get-value old-state id)))))))



;; dynamic state


(def ^:dynamic state-atom)

(defn state-bindings [& options]
  {#'state-atom (atom (apply initialize-state options))})

(defn get! [id specification]
  (get state-atom id specification))

(defn delete-unused-values! [minimum-references-after-previous-delete]
  (delete-unused-values state-atom minimum-references-after-previous-delete))

(defn mark-reference! [id]
  (swap! state-atom mark-reference id))





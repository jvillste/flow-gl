(ns fungl.value-registry
  (:require [clojure.set :as set]
            [clojure.test :refer :all])
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
    (if (not specification)
      state
      #_(throw (ex-info (str "no value found for id " id " and no specification given")
                        {}))
      (initialize-value state id specification))))

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
      (when value
        (on-get id
                value)))

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
        (when-let [destructor (get-in old-state [:specifications id :delete])]
          (destructor (get-value old-state id)))))))



;; dynamic state


(def ^:dynamic state-atom)

(defn state-bindings [& options]
  {#'state-atom (atom (apply initialize-state options))})

(defn get!
  ([id specification]
   (get state-atom id specification))
  
  ([id]
   (get state-atom id nil)))

(defn delete-unused-values! [minimum-references-after-previous-delete]
  (delete-unused-values state-atom minimum-references-after-previous-delete))

(defn mark-reference! [id]
  (swap! state-atom mark-reference id))


(defmacro get-fn! [id arguments & body]
  `(get! ~id {:create (fn []
                        (fn [~@arguments]
                          ~@body))}))

(deftest test-create-handler-with-fixed-context
  (with-bindings (state-bindings)
    (let [x 1]
      (is (= (get-fn! :text-area-keyboard-event-handler
                      [y]
                      [x y])
             
             (get-fn! :text-area-keyboard-event-handler
                      [y]
                      [x y])))

      (is (= (System/identityHashCode (get-fn! :text-area-keyboard-event-handler
                                               [y]
                                               [x y]))
             
             (System/identityHashCode (get-fn! :text-area-keyboard-event-handler
                                               [y]
                                               [x y]))))

      (is (= ((get-fn! :text-area-keyboard-event-handler
                       [y]
                       [x y])
              2)
             
             [1 2])))))

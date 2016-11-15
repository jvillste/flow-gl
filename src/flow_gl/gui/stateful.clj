(ns flow-gl.gui.stateful
  (:require [clojure.set :as set])
  (:use [clojure.test]))

(defn initialize-state [& {:keys [delete-after-calls] :or {delete-after-calls nil}}]
  (if delete-after-calls
    {:delete-after-calls delete-after-calls}
    {}))

#_(defn delete-unused-states [all-stateful-state]
    (reduce (fn [all-stateful-state id])
            all-stateful-state
            () id))

(def states-key :states)
(def called-key :called)
(def destructors-key :destructors)
(def calls-after-delete-key :calls-after-delete)

(defn set-stateful-state [all-stateful-state id stateful-state]
  (assoc-in all-stateful-state
            [states-key id]
            stateful-state))

(defn update-stateful-state [all-stateful-state id function & arguments]
  (update-in all-stateful-state
             [states-key id]
             (fn [stateful-state]
               (apply function
                      stateful-state
                      arguments))))

(defn get-stateful-state [all-stateful-state id]
  (get-in all-stateful-state [states-key id]))

#_(defn call-with-state [all-stateful-state-atom initialize-state function id & arguments]
    (when (not (contains? @all-stateful-state-atom id))
      (swap! all-stateful-state-atom
             assoc id (assoc (initialize-state)
                             ::id id)))

    (apply function
           (get @all-stateful-state-atom id) id arguments))

(defn remove-stateful [all-stateful-state id]
  (-> all-stateful-state
      (update-in [states-key] dissoc id)
      (update-in [destructors-key] dissoc id)))

(deftest remove-stateful-test
  (is (= {:states {} 
          :destructors {},
          :called #{}}
         (remove-stateful {:states {:stateful-1 :stateful-1-state}  
                           :destructors {:stateful-1 :stateful-1-destructor},
                           :called #{}}
                          :stateful-1))))

(defn delete-unused-states [all-stateful-state]

  (let [ids-to-be-deleted (set/difference (apply hash-set (keys (states-key all-stateful-state)))
                                          (called-key all-stateful-state))]
    (println "deleting states" ids-to-be-deleted)

    (doseq [id ids-to-be-deleted]
      (when-let [destructor (get-in all-stateful-state [destructors-key id])]
        (destructor (get-stateful-state all-stateful-state
                                        id))))

    (-> (reduce remove-stateful
                all-stateful-state
                ids-to-be-deleted)
        (assoc called-key #{})
        (assoc calls-after-delete-key 0))))


(defn delete-unused-states-after [all-stateful-state calls]
  (if (> (calls-after-delete-key all-stateful-state)
         calls)
    (delete-unused-states all-stateful-state)
    all-stateful-state))


(defn call-with-state-atom [all-stateful-state id initialize-state delete-state function & arguments]
  (let [stateful-state-atom (atom (or (get-stateful-state all-stateful-state id)
                                      (initialize-state)))
        result (apply function
                      stateful-state-atom
                      arguments)]
    [(-> all-stateful-state
         (set-stateful-state id @stateful-state-atom)
         (assoc-in [destructors-key id] delete-state)
         (update called-key (fnil conj #{}) id)
         (update calls-after-delete-key (fnil inc 0))
         (cond-> (:delete-after-calls all-stateful-state)
           (delete-unused-states-after (:delete-after-calls all-stateful-state))))
     result]))

(deftest call-with-state-atom-test
  (let [initializer (fn [] 1)
        destructor (fn [state])
        function (fn [state-atom] (swap! state-atom inc))]
    (is (= [{:states {:stateful-1 2} 
             :destructors {:stateful-1 destructor},
             :called #{:stateful-1}
             :calls-after-delete 1}
            2]

           (-> (initialize-state)
               (call-with-state-atom :stateful-1
                                     initializer
                                     destructor
                                     function))))))



(deftest delete-unused-states-test
  (let [deleted-atom (atom false)
        initializer (fn [] 1)
        destructor (fn [state]
                     (reset! deleted-atom true))
        function (fn [state-atom] (swap! state-atom inc))]
    (let [[all-stateful-state result] (call-with-state-atom (initialize-state)
                                                            :stateful-1
                                                            initializer
                                                            destructor
                                                            function)
          [all-stateful-state result] (call-with-state-atom all-stateful-state
                                                            :stateful-2
                                                            initializer
                                                            destructor
                                                            function)
          all-stateful-state (delete-unused-states all-stateful-state)]
      
      (is (= {:states {:stateful-1 2
                       :stateful-2 2}
              :destructors {:stateful-1 destructor
                            :stateful-2 destructor},
              :called #{}
              :calls-after-delete 0}
             all-stateful-state))
      
      (is (= false @deleted-atom))

      (let [[all-stateful-state result] (call-with-state-atom all-stateful-state
                                                              :stateful-2
                                                              initializer
                                                              destructor
                                                              function)]
        (is (= {:states {:stateful-1 2
                         :stateful-2 3}
                :destructors {:stateful-1 destructor
                              :stateful-2 destructor},
                :called #{:stateful-2}
                :calls-after-delete 1}
               all-stateful-state))
        
        (is (= {:states {:stateful-2 3} 
                :destructors {:stateful-2 destructor},
                :called #{}
                :calls-after-delete 0}
               (delete-unused-states all-stateful-state)))

        (is (= true @deleted-atom))))))




;; dynamic state

(def ^:dynamic all-stateful-state-atom)

(defn state-bindings [& options]
  {#'all-stateful-state-atom (atom (apply initialize-state options))})


(defn update-stateful-state! [id function & arguments]
  (swap! all-stateful-state-atom
         (fn [all-stateful-state]
           (apply update-stateful-state
                  all-stateful-state id function arguments))))

#_(defn stateful-state! [id]
    (stateful-state @all-stateful-state-atom id))

#_(defn call-with-state! [id arguments initialize-state function]
    (apply call-with-state
           all-stateful-state-atom initialize-state function id arguments))

(defn call-with-state-atom! [id initialize-state delete-state function & arguments]
  (let [[all-stateful-state result] (apply call-with-state-atom
                                           @all-stateful-state-atom id initialize-state delete-state function arguments)]
    (reset! all-stateful-state-atom all-stateful-state)
    result))

#_(defn delete-unused-states-after! [calls]
    (swap! all-stateful-state-atom delete-unused-states calls))

(ns flow-gl.gui.stateful
  (:require [clojure.set :as set]
            [fungl.depend :as depend])
  (:use [clojure.test]))

(defn initialize-state [& {:keys [delete-after-calls] :or {delete-after-calls nil}}]
  {:delete-after-calls delete-after-calls})

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


(defn get-or-initialize-stateful-state [all-stateful-state id initialize-state]
  (or (get-stateful-state all-stateful-state id)
      (initialize-state)))

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
    (println "deleting states" ids-to-be-deleted
             "called was" (called-key all-stateful-state))

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

(defn register-call [all-stateful-state id delete-state new-stateful-state]
  (-> all-stateful-state
      (set-stateful-state id new-stateful-state)
      (assoc-in [destructors-key id] delete-state)
      (update called-key (fnil conj #{}) id)
      (update calls-after-delete-key (fnil inc 0))
      (cond-> (:delete-after-calls all-stateful-state)
        (delete-unused-states-after (:delete-after-calls all-stateful-state)))))

(defn call-with-state-atom [all-stateful-state id initialize-state delete-state function & arguments]
  (let [stateful-state-atom (atom (get-or-initialize-stateful-state all-stateful-state id initialize-state))
        result (apply function
                      stateful-state-atom
                      arguments)]
    [(register-call all-stateful-state
                    id
                    delete-state
                    @stateful-state-atom)     
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

(defn call-with-state-atoms [all-stateful-state stateful-specifications function & arguments]
  #_(println "call with state atoms" function)
  (let [add-state-atom (fn [{:keys [id kind initialize-state] :as stateful-specification}]
                         (assoc stateful-specification
                                :state-atom (atom (get-or-initialize-stateful-state all-stateful-state
                                                                                    [id kind]
                                                                                    initialize-state))))
        stateful-specs-with-state-atoms (map add-state-atom stateful-specifications)
        result (apply function (concat (map :state-atom stateful-specs-with-state-atoms)
                                       arguments))]
    [(reduce (fn [all-stateful-state {:keys [id kind state-atom delete-state]}]
               (register-call all-stateful-state
                              [id kind] 
                              delete-state
                              @state-atom))
             all-stateful-state
             stateful-specs-with-state-atoms)
     result]))


(deftest call-with-state-atoms-test
  (let [stateful-initialize-state (fn [] 1)
        stateful-delete-state (fn [state])
        stateful-specification {:initialize-state  stateful-initialize-state :delete-state stateful-delete-state}
        all-stateful-state (initialize-state)]
    (is (= [{:states {[:stateful-1 nil] 6, [:stateful-2 nil] 1},
             :destructors
             {[:stateful-1 nil] stateful-delete-state
              [:stateful-2 nil] stateful-delete-state},
             :called #{[:stateful-1 nil] [:stateful-2 nil]},
             :calls-after-delete 2}
            6]
           (call-with-state-atoms all-stateful-state
                                  [(assoc stateful-specification :id :stateful-1)
                                   (assoc stateful-specification :id :stateful-2)]
                                  (fn [stateul-1-state-atom stateful-2-state-atom value]
                                    (swap! stateul-1-state-atom + value)
                                    @stateul-1-state-atom)
                                  5)))))

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

(defn stateful-state! [id stateful-specification]
  (or (get-stateful-state @all-stateful-state-atom id)
      (do (swap! all-stateful-state-atom
                 set-stateful-state
                 id
                 ((:initialize-state stateful-specification)))
          (get-stateful-state @all-stateful-state-atom id))))

(defn reducer! [id]
  (or (get-in @all-stateful-state-atom [:reducers id])
      (do (swap! all-stateful-state-atom
                 assoc-in
                 [:reducers id]
                 (fn [function & arguments]
                   (apply update-stateful-state! id function arguments)))
          (get-in @all-stateful-state-atom [:reducers id]))))

(defn state-and-reducer! [id stateful-specification]
  [(stateful-state! id stateful-specification)
   (reducer! id)])

(deftest state-and-reducer!-test
  (with-bindings (state-bindings)
    (let [stateful-specification {:initialize-state (fn [] 1)}
          [state reduce!] (state-and-reducer! :foo stateful-specification)]
      (is (= 1 state))
      (reduce! inc)
      (is (= 2 (stateful-state! :foo stateful-specification))))))

#_(defn call-with-state! [id arguments initialize-state function]
    (apply call-with-state
           all-stateful-state-atom initialize-state function id arguments))

(defn call-with-state-atom! [id initialize-state delete-state function & arguments]
  (let [[all-stateful-state result] (apply call-with-state-atom
                                           @all-stateful-state-atom id initialize-state delete-state function arguments)]
    
    (reset! all-stateful-state-atom all-stateful-state)
    
    (depend/add-dependency {:type ::stateful
                            :id id}
                           (stateful-state! id {:initialize-state initialize-state}))
    result))


(defn call-with-state-atoms! [stateful-specifications-and-ids function & arguments]
  (let [stateful-specifications-with-ids (->> (partition 2 stateful-specifications-and-ids)
                                              (map (fn [[id stateful-specification]]
                                                     (assoc stateful-specification
                                                            :id id)))) 
        result-atom (atom nil)]

    (swap! all-stateful-state-atom
           (fn [all-stateful-state]
             (let [[all-stateful-state result] (apply call-with-state-atoms
                                                      all-stateful-state stateful-specifications-with-ids function arguments)]
               (reset! result-atom result)
               all-stateful-state)))
    
    (doseq [[id stateful-specification] (partition 2 stateful-specifications-and-ids)]
      (depend/add-dependency {:type ::stateful
                              :id [id (:kind stateful-specification)]}
                             (stateful-state! [id (:kind stateful-specification)]
                                              stateful-specification)))
    
    @result-atom))


(deftest call-with-state-atoms!-test
  (binding [all-stateful-state-atom (atom (initialize-state))]
    (let [stateful-initialize-state (fn [] 1)
          stateful-delete-state (fn [state])
          stateful-specification {:initialize-state  stateful-initialize-state :delete-state stateful-delete-state}
          all-stateful-state (initialize-state)]
      (is (= 6
             (call-with-state-atoms! [:stateful-1 stateful-specification
                                      :stateful-2 stateful-specification]
                                     (fn [stateul-1-state-atom stateful-2-state-atom value]
                                       (swap! stateul-1-state-atom + value)
                                       @stateul-1-state-atom)
                                     5)))

      (is (= {:states {[:stateful-1 nil] 6, [:stateful-2 nil] 1},
              :destructors
              {[:stateful-1 nil] stateful-delete-state
               [:stateful-2 nil] stateful-delete-state},
              :called #{[:stateful-1 nil] [:stateful-2 nil]},
              :calls-after-delete 2}
             @all-stateful-state-atom))

      (is (= 11
             (call-with-state-atoms! [:stateful-1 stateful-specification
                                      :stateful-2 stateful-specification]
                                     (fn [stateul-1-state-atom stateful-2-state-atom value]
                                       (swap! stateul-1-state-atom + value)
                                       @stateul-1-state-atom)
                                     5))))))

(defmacro with-state-atoms! [symbols-ids-and-stateful-specifications & body]
  (let [ids-and-stateful-specifications (mapcat (fn [[_ id stateful-specification]]
                                                  [id stateful-specification])
                                                (partition 3 symbols-ids-and-stateful-specifications))
        symbols (map (fn [[sym id stateful-specification]]
                       sym)
                     (partition 3 symbols-ids-and-stateful-specifications))]
    `(call-with-state-atoms! [~@ids-and-stateful-specifications]
                             (fn [~@symbols]
                               ~@body))))

(deftest with-state-atoms!-test
  (binding [all-stateful-state-atom (atom (initialize-state))]
    (let [stateful-initialize-state (fn [] 1)
          stateful-delete-state (fn [state])
          stateful-specification {:initialize-state  stateful-initialize-state :delete-state stateful-delete-state}
          all-stateful-state (initialize-state)]
      (is (= 6
             (with-state-atoms! [stateul-1-state-atom :stateful-1 stateful-specification
                                 stateful-2-state-atom :stateful-2 stateful-specification]
               
               (swap! stateul-1-state-atom + 5)
               @stateul-1-state-atom)))

      (is (= {:states {[:stateful-1 nil] 6, [:stateful-2 nil] 1},
              :destructors
              {[:stateful-1 nil] stateful-delete-state
               [:stateful-2 nil] stateful-delete-state},
              :called #{[:stateful-1 nil] [:stateful-2 nil]},
              :calls-after-delete 2}
             @all-stateful-state-atom))
      (is (= 11
             (with-state-atoms! [stateul-1-state-atom :stateful-1 stateful-specification
                                 stateful-2-state-atom :stateful-2 stateful-specification]
               
               (swap! stateul-1-state-atom + 5)
               @stateul-1-state-atom))))))

(defmethod depend/current-value ::stateful [dependency]
  (get-stateful-state @all-stateful-state-atom (:id dependency)))


(defmethod depend/dependency-added ::stateful [dependency]
  (swap! all-stateful-state-atom
         update called-key (fnil conj #{}) (:id dependency))

  (println "dependency added" (:id dependency)
           (get @all-stateful-state-atom
                called-key)))

#_(defn delete-unused-states-after! [calls]
    (swap! all-stateful-state-atom delete-unused-states calls))



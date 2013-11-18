(ns flow-gl.dataflow.base-dataflow
  (:require [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            [flow-gl.dataflow.dataflow :as dataflow]
            clojure.set
            [slingshot.slingshot :as slingshot]
            [clojure.data.priority-map :as priority-map])
  (:use clojure.test
        flow-gl.threading))

;; DEBUG

(defn cell-to-string [dataflow cell]
  (str cell " (height: " (get-in dataflow [::heights cell]) ") = " (if (contains? (::storage dataflow) cell)
                                                                     #_(apply str (take 100 (str (get dataflow cell))))
                                                                     (str (get (::storage dataflow) cell))
                                                                     "UNDEFINED!")
       (if (empty? (get-in dataflow [::dependencies cell]))
         ""
         (str " depends on " (reduce (fn [string cell]
                                       (str string " " cell (if (contains? (::storage dataflow) cell)
                                                              ""
                                                              " = UNDEFINED! ")))
                                     ""
                                     (get-in dataflow [::dependencies cell]))))))

;; DEPENDANTS

(defn set-dependencies [dataflow dependant dependencies]
  (assoc-in dataflow [::dependencies dependant] dependencies))

                                        ; TODO: Make this efficient
(defn dependents [dataflow cell]
  (filter #(contains? (get-in dataflow [::dependencies %])
                      cell)
          (keys (::dependencies dataflow))))


;; ACCESS

(defn is-defined? [dataflow cell]
  (contains? (::functions dataflow)
             cell))

(defn unlogged-get-value [dataflow cell]
  (get-in dataflow [::storage cell]))

(defn get-value [dataflow cell]
  (if (and (is-defined? dataflow cell)
           (not (= (get dataflow cell)
                   ::undefined)))
    (logged-access/get (::storage dataflow) cell)
    (do (logged-access/add-read cell)
        (slingshot/throw+ {:type ::undefined-value} (str "Undefined value: " cell)))))

(defn cells [dataflow]
  (keys (::storage dataflow)))


;; DEFINE

(defn height [dataflow dependencies]
  (+ 1
     (apply max (-> (map #(get-in dataflow [::heights %] 0)
                         dependencies)
                    (conj -1)))))

(defn undefine [dataflow cell]
  (debug/debug :dataflow "undefining" cell)
  (-> (update-in dataflow [::functions] dissoc cell)
      (dataflow/set-dependencies cell #{})
      (update-in [::changed-cells] conj cell)
      (update-in [::storage] dissoc cell)))

(defn declare-changed [dataflow cell]
  (if (keyword? cell)
    (flow-gl.debug/debug :dataflow "Set " cell " = " (apply str (take 100 (str (dataflow/unlogged-get-value dataflow cell)))))
    (flow-gl.debug/debug :dataflow "Set " cell))


  (-> (reduce (fn [dataflow dependent]
                (assoc-in dataflow [::need-to-be-updated dependent] (get-in dataflow [::heights cell])))
              dataflow
              (dependents dataflow cell))
      (update-in [::changed-cells] conj cell)))

(defn update-cell [dataflow cell]
  (logged-access/with-access-logging
    (let [old-value (dataflow/unlogged-get-value dataflow cell)
          new-value (slingshot/try+ ((get-in dataflow [::functions cell]) dataflow)
                                    (catch [:type ::undefined-value]
                                        _ ::undefined))
          new-height (height dataflow @logged-access/reads)]

      (-> dataflow
          (assoc-in [::storage cell] new-value)
          (dataflow/set-dependencies cell @logged-access/reads)
          (assoc-in [::heights cell] new-height)
          (when-> (not (= old-value new-value))
                  (dataflow/declare-changed cell))

          (when-> (= new-value ::undefined)
                  (as-> dataflow
                        (do (println "Warning: " (cell-to-string dataflow cell))
                            (flow-gl.debug/debug :dataflow "Warning: " (cell-to-string dataflow cell)))))))))



(defn define
  ([dataflow cell function & keys-and-functions]
     (apply define
            (define dataflow cell function)
            keys-and-functions))

  ([dataflow cell function]
     #_(println "define to " cell function)
     (let [function (if (fn? function)
                      function
                      (fn [dataflow] function))
           old-value (get-in dataflow [::storage cell])]

       (-> dataflow
           (assoc-in [::functions cell] function)
           (dataflow/update-cell cell)))))

;; UPDATE


(defn changes [dataflow]
  (::changed-cells dataflow))

(defn reset-changes [dataflow]
  (assoc dataflow
    ::changed-cells #{}))

(defn propagate-changes [dataflow]
  (flow-gl.debug/do-debug :dataflow "propagate changes " (vec (map first (::need-to-be-updated dataflow))))
  (let [dataflow (reduce (fn [dataflow [cell priority]]
                           (let [old-value (get-value dataflow cell)]
                             (-> dataflow
                                 (dataflow/update-cell cell)
                                 (update-in [::need-to-be-updated] dissoc cell))))
                         dataflow
                         (::need-to-be-updated dataflow))]
    (if (empty? (::need-to-be-updated dataflow))
      dataflow
      (recur dataflow))))


;; PROTOCOL

(def dataflow-implementation {:define define
                              :undefine undefine
                              :get-value get-value
                              :unlogged-get-value unlogged-get-value
                              :is-defined? is-defined?
                              :update-cell update-cell
                              :propagate-changes propagate-changes
                              :set-dependencies set-dependencies
                              :declare-changed declare-changed
                              :cells cells
                              :changes changes
                              :reset-changes reset-changes})

(defrecord BaseDataflow [])

(extend BaseDataflow
  dataflow/Dataflow
  dataflow-implementation)


;; CREATE

(defn create []
  (map->BaseDataflow {::changed-cells #{}
                      ::need-to-be-updated (priority-map/priority-map)
                      ::heights {}
                      ::storage {}}))


;; DEBUG

(defn describe-cells [dataflow cells]
  (for [cell cells]
    (cell-to-string dataflow cell)))

(defn describe-dataflow [dataflow]
  (describe-cells dataflow
                  (sort (keys (::functions dataflow)))))

(defn debug-dataflow [dataflow]
  (debug/debug-all :dataflow (describe-dataflow dataflow))
  dataflow)


;; TESTS

(debug/reset-log)

(comment
  (debug/set-active-channels :dataflow))

(deftest get-value-or-nil-test
  (is (= (-> (create)
             (define :foo 2)
             (define :foo2 (fn [dataflow] (+ 1 (get-value dataflow :foo))))
             (define :foo 3)
             (propagate-changes)
             (get-value :foo2))
         4)))

(debug/write-log)

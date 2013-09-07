(ns flow-gl.triple-dataflow
  (:require [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            clojure.set
            [slingshot.slingshot :as slingshot]
            [clojure.data.priority-map :as priority-map])
  (:use clojure.test
        flow-gl.threading))



(comment (defprotocol TripleStore
           (set [triple-store subject predicate object])
           (get [triple-store subject predicate]))

         (defprotocol KeyValueStore
           (set-value [key-value-store cell value])
           (get-value [key-value-store cell value]))

         (defprotocol Dataflow
           (define [dataflow cell function-or-value]))

         (defn create [key-value-store]))




;; DEPENDANTS

(defn set-dependencies [dataflow dependant dependencies]
  (if (not= (get-in dataflow [::dependencies dependant])
            dependencies)
    (assoc-in dataflow [::dependencies dependant] dependencies)
    dataflow))

(defn dependants [dataflow cell]
  (filter #(contains? (get-in dataflow [::dependencies %])
                      cell)
          (keys (::dependencies dataflow))))



;; ACCESS

(defn is-defined? [dataflow cell]
  (contains? (::functions dataflow)
             cell))

(defn get-value [dataflow cell]
  (if (and (is-defined? dataflow cell)
           (not (= (get dataflow cell)
                   ::undefined)))
    (logged-access/get (::storage dataflow) cell)
    (do (logged-access/add-read cell)
        (slingshot/throw+ {:type ::undefined-value} (str "Undefined value: " cell)))))

(defn maybe-get-value [dataflow cell]
  (if (is-defined? dataflow cell)
    (get-value cell)
    nil))





;; DEBUG

(defn function-to-string [dataflow cell]
  (str cell " (height: " (get-in dataflow [::heights cell]) ") = " (if (contains? dataflow cell)
                                                                   #_(apply str (take 100 (str (get-value dataflow cell))))
                                                                   (str (get-value dataflow cell))
                                                                   "UNDEFINED!")
       (if (empty? (get-in dataflow [::dependencies cell]))
         ""
         (str " depends on " (reduce (fn [string cell]
                                       (str string " " cell (if (contains? dataflow cell)
                                                             ""
                                                             " = UNDEFINED! ")))
                                     ""
                                     (get-in dataflow [::dependencies cell]))))))

;; CREATE

(defn create [storage]
  {::changed-keys #{}
   ::need-to-be-updated (priority-map/priority-map)
   ::heights {}
   ::storage storage})



;; DEFINE

(defn height [dataflow dependencies]
  (+ 1
     (apply max (-> (map #(get-in dataflow [::heights %] 0)
                         dependencies)
                    (conj -1)))))

(defn undefine [dataflow cell]
  (do
    (debug/debug :dataflow "undefining" cell)
    (-> (update-in [::functions] dissoc cell)
        (set-dependencies cell #{})
        (update-in [::changed-keys] conj cell)
        (update-in [::storage] dissoc cell))))

(defn undefine-many [dataflow keys]
  (reduce (fn [dataflow cell]
            (undefine dataflow cell))
          dataflow
          keys))

(defn declare-changed [dataflow cell]
  (update-in dataflow [::changed-keys] conj cell))

(defn update-value [dataflow cell]
  (logged-access/with-access-logging
    (let [old-value (get-in dataflow [::storage cell])
          new-value (slingshot/try+ ((get-in dataflow [::functions cell]) dataflow)
                                    (catch [:type ::undefined-value] _
                                      ::undefined))
          changed (not (= old-value new-value))]

      (-> dataflow
          (assoc-in [::storage cell] new-value)
          (set-dependencies cell @logged-access/reads)
          ((fn [dataflow]
             (assoc-in dataflow [::heights cell] (height dataflow @logged-access/reads))))
          (when-> changed (declare-changed cell))
          (when-> (= new-value ::undefined)
                  ((fn [dataflow]
                     (println "Warning: " (function-to-string dataflow cell))
                     (flow-gl.debug/debug :dataflow "Warning: " (function-to-string dataflow cell))
                     dataflow)))))))



(defn schedule-for-update [dataflow cell]
  (assoc-in dataflow [::need-to-be-updated cell] (get-in dataflow [::heights cell])))


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
           (update-value cell)
           ((fn [dataflow] (if (not (= old-value
                                       (get-value dataflow cell)))
                             (do
                               (flow-gl.debug/debug :dataflow "defined " cell " = " (apply str (take 100 (str (get-in dataflow [::storage cell])))))
                               (reduce schedule-for-update
                                       dataflow
                                       (dependants dataflow cell)))
                             dataflow)))))))

(defn initialize [dataflow & keys-and-functions]
  (let [keys-and-functions (->> keys-and-functions
                                (partition 2)
                                (filter (fn [[cell function]]
                                          (not (contains? dataflow cell))))
                                (apply concat))]
    (when (not (empty? keys-and-functions))
      (apply define dataflow keys-and-functions))))

(defn get-value-or-initialize [dataflow cell default]
  (when (not (is-defined? dataflow cell))
    (initialize dataflow cell default))
  (get-value dataflow cell))

(defn apply-to-value [dataflow cell function & arguments]
  (define dataflow cell (apply function (get-value dataflow cell) arguments)))


;; UPDATE


(defn changes [dataflow]
  (::changed-keys dataflow))

(defn reset-changes [dataflow]
  (assoc dataflow
    ::changed-keys #{}))

(defn propagate-changes [dataflow]
  (flow-gl.debug/do-debug :dataflow "propagate changes " (vec (map first (::need-to-be-updated dataflow))))
  (let [dataflow (reduce (fn [dataflow [cell priority]]
                           (let [old-value (get-value dataflow cell)]
                             (-> dataflow
                                 (update-value cell)
                                 (update-in [::need-to-be-updated] dissoc cell)
                                 ((fn [dataflow] (if (not (= old-value
                                                             (get-value dataflow cell)))
                                                   (do (flow-gl.debug/do-debug :dataflow "updated cell " cell " = " (apply str (take 100 (str (get-value dataflow cell)))))
                                                       (reduce schedule-for-update dataflow (dependants dataflow cell)))
                                                   dataflow))))))
                         dataflow
                         (::need-to-be-updated dataflow))]
    (if (empty? (::need-to-be-updated dataflow))
      dataflow
      (recur dataflow))))


;; TESTS

(debug/reset-log)

(comment 
(debug/set-active-channels :dataflow))

(deftest get-value-or-nil-test
  (is (= (-> (create {})
             (define :foo 2)
             (define :foo2 (fn [dataflow] (+ 1 (get-value dataflow :foo))))
             (define :foo 3)
             (propagate-changes)
             (get-value :foo2))
         4)))

(run-tests)
(debug/write-log)

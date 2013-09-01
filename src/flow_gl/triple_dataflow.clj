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
           (set-value [key-value-store key value])
           (get-value [key-value-store key value]))

         (defprotocol Dataflow
           (define [dataflow key function-or-value]))

         (defn create [key-value-store]))




;; DEPENDANTS

(defn set-dependencies [dataflow dependant dependencies]
  (if (not= (get-in dataflow [::dependencies dependant])
            dependencies)
    (assoc-in dataflow [::dependencies dependant] dependencies)
    dataflow))

(defn dependants [dataflow key]
  (filter #(contains? (get-in dataflow [::dependencies %])
                      key)
          (keys (::dependencies dataflow))))


;; DEBUG

(defn function-to-string [dataflow key]
  (str key " (height: " (get-in dataflow [::heights key]) ") = " (if (contains? dataflow key)
                                                                   #_(apply str (take 100 (str (get dataflow key))))
                                                                   (str (get dataflow key))
                                                                   "UNDEFINED!")
       (if (empty? (get-in dataflow [::dependencies key]))
         ""
         (str " depends on " (reduce (fn [string key]
                                       (str string " " key (if (contains? dataflow key)
                                                             ""
                                                             " = UNDEFINED! ")))
                                     ""
                                     (get-in dataflow [::dependencies key]))))))

;; UTILITIES

(defn multimap-add
  "Adds key-value pairs to the multimap."
  ([mm k v]
     (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs]
     (apply multimap-add (multimap-add mm k v) kvs)))

(defn multimap-del
  "Removes key-value pairs from the multimap."
  ([mm k v]
     (assoc mm k (disj (get mm k) v)))
  ([mm k v & kvs]
     (apply multimap-del (multimap-del mm k v) kvs)))


;; CREATE

(defn create []
  {::changed-keys #{}
   ::need-to-be-updated (priority-map/priority-map)
   ::heights {}})



;; DEFINE

(defn height [dataflow dependencies]
  (+ 1
     (apply max (-> (map #(get-in dataflow [::heights %] 0)
                         dependencies)
                    (conj -1)))))

(defn undefine [dataflow key]
  (do
    (debug/debug :dataflow "undefining" key)
    (-> (update-in [::functions] dissoc key)
        (set-dependencies key #{})
        (update-in [::changed-keys] conj key)
        (dissoc key))))

(defn undefine-many [dataflow keys]
  (reduce (fn [dataflow key]
            (undefine dataflow key))
          dataflow
          keys))

(defn declare-changed [dataflow key]
  (update-in dataflow [::changed-keys] conj key))

(defn update-value [dataflow key]
  (logged-access/with-access-logging
    (let [old-value (get dataflow key)
          new-value (slingshot/try+ ((get-in dataflow [::functions key]) dataflow)
                                    (catch [:type ::undefined-value] _
                                      ::undefined))
          changed (not (= old-value new-value))]

      (-> dataflow
          (when-> (not (= new-value ::undefined))
                  (assoc key new-value))
          (set-dependencies key @logged-access/reads)
          ((fn [dataflow]
             (assoc-in dataflow [::heights key] (height dataflow @logged-access/reads))))
          (when-> changed (declare-changed key))
          (when-> (= new-value ::undefined)
                  ((fn [dataflow]
                     (println "Warning: " (function-to-string dataflow key))
                     (flow-gl.debug/debug :dataflow "Warning: " (function-to-string dataflow key))
                     dataflow)))))))

(defn is-defined? [dataflow key]
  (contains? (::functions dataflow)
             key))

(defn schedule-for-update [dataflow key]
  (assoc-in dataflow [::need-to-be-updated key] (get-in dataflow [::heights key])))


(defn define
  ([dataflow key function & keys-and-functions]
     (apply define
            (define dataflow key function)
            keys-and-functions))

  ([dataflow key function]
     (println "define to " key function)
     (let [function (if (fn? function)
                      function
                      (fn [dataflow] function))
           old-value (get dataflow key)]

       (-> dataflow
           (assoc-in [::functions key] function)
           (update-value key)
           ((fn [dataflow] (if (not (= old-value
                                       (get dataflow key)))
                             (do
                               (flow-gl.debug/debug :dataflow "defined " key " = " (apply str (take 100 (str (get dataflow key)))))
                               (reduce schedule-for-update
                                       dataflow
                                       (dependants dataflow key)))
                             dataflow)))))))

(defn initialize [dataflow & keys-and-functions]
  (let [keys-and-functions (->> keys-and-functions
                                (partition 2)
                                (filter (fn [[key function]]
                                          (not (contains? dataflow key))))
                                (apply concat))]
    (when (not (empty? keys-and-functions))
      (apply define dataflow keys-and-functions))))



;; UPDATE


(defn changes [dataflow]
  (::changed-keys dataflow))

(defn reset-changes [dataflow]
  (assoc dataflow
    ::changed-keys #{}))

(defn propagate-changes [dataflow]
  (flow-gl.debug/do-debug :dataflow "propagate changes " (vec (map first (::need-to-be-updated dataflow))))
  (let [dataflow (reduce (fn [dataflow [key priority]]
                           (let [old-value (get dataflow key)]
                             (-> dataflow
                                 (update-value key)
                                 (update-in [::need-to-be-updated] dissoc key)
                                 ((fn [dataflow] (if (not (= old-value
                                                             (get dataflow key)))
                                                   (do (flow-gl.debug/do-debug :dataflow "updated key " key " = " (apply str (take 100 (str (get dataflow key)))))
                                                       (reduce schedule-for-update dataflow (dependants dataflow key)))
                                                   dataflow))))))
                         dataflow
                         (::need-to-be-updated dataflow))]
    (if (empty? (::need-to-be-updated dataflow))
      dataflow
      (recur dataflow))))



;; ACCESS

(defn get-value [dataflow key]
  (if (contains? dataflow key)
    (logged-access/get dataflow key)
    (do (logged-access/add-read key)
        (slingshot/throw+ {:type ::undefined-value} (str "Undefined value: " key)))))

(defn maybe-get-value [dataflow key]
  (if (is-defined? dataflow key)
    (get-value key)
    nil))

(defn get-value-or-initialize [dataflow key default]
  (when (not (is-defined? dataflow key))
    (initialize dataflow key default))
  (get-value dataflow key))

(defn apply-to-value [dataflow key function & arguments]
  (define dataflow key (apply function (get dataflow key) arguments)))



;; TESTS

(debug/reset-log)
(debug/set-active-channels :dataflow)

(deftest get-value-or-nil-test
  (is (= (-> (create)
             (define :foo 2)
             (define :foo2 (fn [dataflow] (+ 1 (get-value dataflow :foo))))
             (define :foo 3)
             (propagate-changes)
             (get-value :foo2))
         4)))

(run-tests)
(debug/write-log)

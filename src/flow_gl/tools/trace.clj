(ns flow-gl.tools.trace
  (:require [flow-gl.debug :as debug])
  (:use clojure.test))

(defn create-state []
  {:root-calls []
   :open-calls {}
   :current-calls {}})

(defn add-entry [trace entry]
  (println trace)
  (if (:call-started entry)

    (let [opening-call (-> (select-keys entry [:call-id :function-name :arguments :thread])
                           (assoc :parent (get-in trace [:current-calls (:thread entry)])
                                  :child-calls []
                                  :call-started (:time entry)))]
      (-> trace
          (update-in [:open-calls] assoc (:call-id entry) opening-call)
          (update-in [:current-calls] assoc (:thread entry) (:call-id entry))))

    (let [ending-call (-> (get-in trace [:open-calls (:call-id entry)])
                          (assoc :call-ended (:time entry)
                                 :result (:result entry)))]
      (let [trace (-> trace
                      (update-in [:open-calls] dissoc (:call-id entry))
                      (update-in [:current-calls] assoc (:thread entry) (:parent ending-call)))]
        (if (:parent ending-call)
          (update-in trace [:open-calls (:parent ending-call) :child-calls] conj ending-call)
          (update-in trace [:root-calls] conj ending-call))))))

(deftest add-entry-test
  (is (= nil
         (-> (create-state)
             (add-entry {:thread 1
                         :time 1
                         :call-id 1
                         :arguments [:foo]
                         :function-name 'foo
                         :call-started true})
             (add-entry {:thread 1
                         :time 2
                         :call-id 2
                         :arguments [:foo]
                         :function-name 'bar
                         :call-started true})
             (add-entry {:thread 1
                         :time 3
                         :call-id 2
                         :result :bar})
             (add-entry {:thread 1
                         :time 4
                         :call-id 1
                         :result :bar})))))

(defn trace-fn-call
  [name f arguments]
  (let [call-id (gensym)]
    (debug/add-timed-entry :function-name name
                           :call-id call-id
                           :call-started true
                           :arguments arguments)

    (let [value (apply f arguments)]
      (debug/add-timed-entry :call-id call-id
                             :result value)
      value)))

(defn trace-var*
  ([ns s]
     (trace-var* (ns-resolve ns s)))
  ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)]
       (if (and (ifn? @v) (-> v meta :macro not) (-> v meta ::traced not))
         (let [f @v
               vname (symbol (str ns "/" s))]
           (doto v
             (alter-var-root #(fn tracing-wrapper [& args]
                                (trace-fn-call vname % args)))
             (alter-meta! assoc ::traced f)))))))

(defn trace-ns*
  [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core flow-gl.tools.trace} (.name ns))
      (let [ns-fns (->> ns ns-interns vals (filter (fn [v] (and (-> v var-get fn?)
                                                                (not (-> v meta :macro))))))]
        (doseq [f ns-fns]
          (println (-> f meta :name))
          #_(trace-var* f))))))

(defmacro trace-ns
  "Trace all fns in the given name space."
  [ns]
  `(trace-ns* ~ns))


(defn start-tracer [input-channel]
  #_(.start (Thread. (fn []
                     (gui/start-control (create-profiler-control channel#))))))

(defmacro with-trace [& body]
  `(let [channel# (async/chan 50)]
     (start-tracer channel#)

     (async/>!! channel# {})
     (debug/with-debug-channel channel# ~@body)
     (async/close! channel#)))

(run-tests)

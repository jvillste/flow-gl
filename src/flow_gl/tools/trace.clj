(ns flow-gl.tools.trace
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [flow-gl.debug :as debug]
   [clojure.string :as string]
   [fungl.util :as util]
   [fungl.cache :as cache]))

(defn create-state []
  {:root-calls []
   :open-calls {}
   :current-calls {}})

(defn add-entry [trace entry]
  (if (:clear-trace entry)
    (create-state)
    (if (:call-id entry)
      (if (:call-started entry)

        (let [opening-call (-> (select-keys entry [:call-id :function-symbol :arguments :thread])
                               (assoc :parent (get-in trace [:current-calls (:thread entry)])
                                      :child-calls []
                                      :call-started (:time entry)))]
          (-> trace
              (update-in [:open-calls] assoc (:call-id entry) opening-call)
              (update-in [:current-calls] assoc (:thread entry) (:call-id entry))))

        (let [ending-call (-> (get-in trace [:open-calls (:call-id entry)])
                              (assoc :call-ended (:time entry)
                                     :result (:result entry))
                              (merge (when (:exception entry)
                                       {:exception (:exception entry)})))]
          (let [trace (-> trace
                          (update-in [:open-calls] dissoc (:call-id entry))
                          (update-in [:current-calls] assoc (:thread entry) (:parent ending-call)))]
            (if (:parent ending-call)
              (update-in trace [:open-calls (:parent ending-call) :child-calls] conj ending-call)
              (-> trace
                  (update-in [:root-calls] conj ending-call)
                  (update-in [:root-calls] #(vec (take-last 10 %))))))))
      trace)))

(deftest add-entry-test
  (is (= {:root-calls
          [{:result :foo,
            :call-ended 4,
            :call-started 1,
            :child-calls
            [{:result :bar,
              :call-ended 3,
              :call-started 2,
              :child-calls [],
              :parent 1,
              :thread 1,
              :arguments [:foo],
              :function-symbol 'bar,
              :call-id 2}],
            :parent nil,
            :thread 1,
            :arguments [:foo],
            :function-symbol 'foo,
            :call-id 1}],
          :open-calls {},
          :current-calls {1 nil}}
         (-> (create-state)
             (add-entry {:thread 1
                         :time 1
                         :call-id 1
                         :arguments [:foo]
                         :function-symbol 'foo
                         :call-started true})
             (add-entry {:thread 1
                         :time 2
                         :call-id 2
                         :arguments [:foo]
                         :function-symbol 'bar
                         :call-started true})
             (add-entry {:thread 1
                         :time 3
                         :call-id 2
                         :result :bar})
             (add-entry {:thread 1
                         :time 4
                         :call-id 1
                         :result :foo})))))

(defn trace-fn-call [name f arguments]
  (let [call-id (gensym)]
    (debug/add-timed-entry :function-symbol name
                           :call-id call-id
                           :call-started true
                           :arguments arguments)

    (let [[value exception] (try [(apply f arguments)
                                  nil]
                                 (catch Exception e
                                   [nil e]))]


      (debug/add-timed-entry :call-id call-id
                             :result value
                             :exception exception)

      (when exception
        (throw exception))

      value)))


(defmacro tfn [name arguments & body]
  `(fn ~arguments
     (let [call-id# (gensym)]
       (debug/add-timed-entry :function-symbol '~name
                              :call-id call-id#
                              :call-started true
                              :arguments ~arguments)

       (let [value# (do ~@body)]
         (debug/add-timed-entry :call-id call-id#
                                :result value#)
         value#))))


(defn log [& arguments]
  (let [call-id (gensym)]
    (debug/add-timed-entry :function-symbol nil
                           :call-id call-id
                           :call-started true
                           :arguments (vec arguments))

    (debug/add-timed-entry :call-id call-id
                           :result nil)
    (last arguments)))

(defn log-and-return [value]
  (log value)
  value)


(defn untrace-var
  ([ns s]
   (untrace-var (ns-resolve ns s)))
  ([the-var]
   (let [^clojure.lang.Var the-var (if (var? the-var) the-var (resolve the-var))]
     (if-let [uncached-var (-> the-var meta :fungl.cache/uncached)]
       (untrace-var uncached-var)
       (when ((meta the-var) ::traced)
         (println "untracing" the-var)
         (doto the-var
           (alter-var-root (constantly ((meta the-var) ::traced)))
           (alter-meta! dissoc ::traced)))))))

(defn untrace-ns [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
      (untrace-var f))))

(defn trace-var
  ([ns symbol]
   (trace-var (ns-resolve ns symbol)))
  ([the-var]
   (let [^clojure.lang.Var the-var (if (var? the-var)
                                     the-var
                                     (resolve the-var))
         ns (.ns the-var)
         symbol  (.sym the-var)]
     (if-let [uncached-var (-> the-var meta :fungl.cache/uncached)]
       (do (println "not tracing cahed var" the-var)
           (trace-var uncached-var))
       (when (and (ifn? @the-var)
                  (-> the-var meta :macro not)
                  (-> the-var meta ::traced not))
         (println "tracing" symbol)
         (let [f @the-var
               vname (clojure.core/symbol (str ns "/" symbol))]
           (doto the-var
             (alter-var-root #(fn tracing-wrapper [& args]
                                (trace-fn-call vname % args)))
             (alter-meta! assoc ::traced f))))))))

(defn namespace-function-vars [namespace]
  (->> namespace ns-interns vals (filter (fn [v] (and (-> v var-get fn?)
                                                      (not (-> v meta :macro)))))))

(defn trace-ns [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core flow-gl.tools.trace} (.name ns))
      (let [ns-fns (namespace-function-vars ns)]
        (doseq [f ns-fns]

          (trace-var f))))))

(defn trace-some-from-ns [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core flow-gl.tools.trace} (.name ns))
      (untrace-ns ns)
      (let [ns-fns (namespace-function-vars ns)]
        (doseq [f ns-fns]
          (when (:trace (meta f))
            (trace-var f)))))))


(defn traced?
  "Returns true if the given var is currently traced, false otherwise"
  [v]
  (let [^clojure.lang.Var v (if (var? v) v (resolve v))]
    (-> v meta ::traced nil? not)))

(defn start-tracer [input-channel trace-channel]
  (async/thread (async/go-loop [trace-state (create-state)]
                  (if-let [entry (async/<! input-channel)]
                    (let [new-trace-state (add-entry trace-state entry)]
                      (async/>! trace-channel new-trace-state)
                      (recur new-trace-state))
                    (async/close! trace-channel)))))

(defmacro with-trace-logging [& body]
  `(let [channel# (async/chan)]
     (async/go-loop []
       (when-let [entry# (async/<! channel#)]
         (if (:function-symbol entry#)
           (println (apply str (concat ["(" (:function-symbol entry#) " " ] (interpose " " (:arguments entry#)) [")"])))
           (if-let [arguments# (:arguments entry#)]
             (prn arguments#)))
         (recur)))
     (debug/with-debug-channel channel# ~@body)
     (async/close! channel#)))


(defn trace-implementation [function]
  (let [input-channel (async/chan)
        trace-channel (async/chan)
        go-block-channel (async/go (loop [last-trace-state nil]
                                     (if-let [trace-state (async/<! trace-channel)]
                                       (recur trace-state)
                                       last-trace-state)))]
    (start-tracer input-channel trace-channel)
    (let [result (try (debug/with-debug-channel input-channel (function))
                      (catch Exception exception
                        exception))]

      (async/close! input-channel)

      {:result result
       :trace (async/<!! go-block-channel)})))

(defmacro trace [& body]
  `(trace-implementation (fn [] ~@body)))


(defn- print-call-tree-implementation [level values-atom call]
  (let [value-to-string (fn [value]
                          (let [value-size (util/value-size value)]
                            (if (< 10 value-size)
                              (do (swap! values-atom assoc (hash value) value)
                                  (str "<"value-size " " (hash value) ">"))
                              (pr-str value))))]
    (println (string/join " "
                          (concat [(str (apply str (repeat (* level 2) " "))
                                        (:function-symbol call))]
                                  (map value-to-string (:arguments call))
                                  (when (:result call)
                                    [" -> "
                                     (value-to-string (:result call))])))))

  (run! (partial print-call-tree-implementation
                 (inc level)
                 values-atom)
        (:child-calls call))
  values-atom)

(defn- print-call-tree [values-atom call]
  (print-call-tree-implementation 0
                                  values-atom
                                  call))

(defn with-call-tree-printing-implementation [values-atom function]
  (let [{:keys [trace result]} (trace (function))]
    (doseq [root-call (:root-calls trace)]
      (print-call-tree values-atom
                       root-call))
    result))

(defmacro with-call-tree-printing [values-atom & body]
  `(with-call-tree-printing-implementation ~values-atom
     (fn [] ~@body)))

(comment

  (trace-ns 'flow-gl.tools.trace)

  (do (defn foo [x] {:x x})
      (defn bar [x]
        (log :in-bar x)
        (foo (+ 1 x)))
      (trace-var #'foo)
      (trace-var #'bar))

  (with-trace-logging
    (bar 1))

  (with-call-tree-printing (atom {})
    (bar 1)
    (bar 2))
  )

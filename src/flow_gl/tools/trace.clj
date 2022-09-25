(ns flow-gl.tools.trace
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [flow-gl.debug :as debug]))

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
                                     :result (:result entry)
                                     :exception (:exception entry)))]
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
          [{:result :bar,
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
                         :result :bar})))))

(defn trace-fn-call [name f arguments]
  (let [call-id (gensym)]
    (debug/add-timed-entry :function-symbol name
                           :call-id call-id
                           :call-started true
                           :arguments arguments)

    (let [[value exception] (try [(apply f arguments)
                                  nil]
                                 (catch Exception e
                                   (println "got exception" e)
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
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)
         f  ((meta v) ::traced)]
     (when f
       (doto v
         (alter-var-root (constantly ((meta v) ::traced)))
         (alter-meta! dissoc ::traced))))))

(defn untrace-ns [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
      (untrace-var f))))


(defn trace-var
  ([ns s]
   (trace-var (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)]
     (if (and (ifn? @v)
              (-> v meta :macro not)
              (-> v meta ::traced not))
       (do (println "tracing" s)
           (let [f @v
                 vname (symbol (str ns "/" s))]
             (doto v
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
  (async/thread (async/go-loop [trace (create-state)]
                  (if-let [entry (async/<! input-channel)]
                    (let [new-trace (add-entry trace entry)]
                      (async/>! trace-channel new-trace)
                      (recur new-trace))
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


(defmacro log-to-atom [log-atom & body]
  `(let [input-channel# (async/chan 50)
         trace-channel# (async/chan)]
     (start-tracer input-channel# trace-channel#)
     (async/go-loop []
       (when-let [entry# (async/<! trace-channel#)]
         (swap! ~log-atom conj entry#)
         (recur)))
     (debug/with-debug-channel input-channel# ~@body)
     (async/close! input-channel#)
     ~log-atom))


(defmacro with-trace [& body]
  `(let [input-channel# (async/chan 50)
         trace-channel# (async/chan)]
     (start-tracer input-channel# trace-channel#)
     #_(gui/start-control (create-trace-control input-channel# trace-channel#))
     (start-trace-printer trace-channel#)
     (debug/with-debug-channel input-channel# ~@body)
     #_(Thread/sleep 1000)
     #_(async/close! input-channel#)))


(defn foo [x] {:x x})
(defn bar [x]
  (log :in-bar)
  (foo (+ 1 x)))

(comment

  (do
    (trace-var #'foo)
    (trace-var #'bar)
    (let [log-atom (atom [])]
      (log-to-atom log-atom

                   (bar 1))
      @log-atom)
    #_(let [log-atom (atom [])]
      (debug/with-log log-atom
        (prn debug/dynamic-debug-channel) ;; TODO: remove-me

        (log :log))
      log-atom)
    #_(with-trace
        (bar 1))))

(defn start []
  (do (trace-ns 'flow-gl.tools.trace)
      (trace-var #'foo)
      (trace-var #'bar)

      (with-trace-logging
        (foo 1)
        (bar 1))))

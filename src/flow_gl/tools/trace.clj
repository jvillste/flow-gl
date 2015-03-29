(ns flow-gl.tools.trace
  (:require [flow-gl.debug :as debug]
            [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [controls :as controls]
                         [layout-dsl :as l]
                         [transformer :as transformer]))
  (:use clojure.test))

(defn create-state []
  {:root-calls []
   :open-calls {}
   :current-calls {}})

(defn add-entry [trace entry]
  (println trace)
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
                                 :result (:result entry)))]
      (let [trace (-> trace
                      (update-in [:open-calls] dissoc (:call-id entry))
                      (update-in [:current-calls] assoc (:thread entry) (:parent ending-call)))]
        (if (:parent ending-call)
          (update-in trace [:open-calls (:parent ending-call) :child-calls] conj ending-call)
          (update-in trace [:root-calls] conj ending-call))))))

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

(defn trace-fn-call
  [name f arguments]
  (let [call-id (gensym)]
    (debug/add-timed-entry :function-symbol name
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
          (trace-var* f))))))

(defmacro trace-ns
  "Trace all fns in the given name space."
  [ns]
  `(trace-ns* ~ns))


(defn start-tracer [input-channel trace-channel]
  (.start (Thread. (fn []
                     (async/go-loop [trace (create-state)]
                       (if-let [entry (async/<! input-channel)]
                         (let [new-trace (add-entry trace entry)]
                           (println "got " entry)
                           (async/>! trace-channel new-trace)
                           (recur new-trace))
                         (async/close! trace-channel)))))))



;; UI


(defn text-cell [value]
  (l/margin 1 2 1 2 (controls/text value)))

(defn call-view [call]
  (l/vertically (l/horizontally (text-cell (:function-symbol call))
                                (for [argument (:arguments call)]
                                  (text-cell argument))
                                (text-cell (str " -> " (:result call))))
                (l/margin 0 0 0 10
                          (l/vertically (for [child (:child-calls call)]
                                          (call-view child))))))


(defn trace-view [view-context {:keys [trace]}]
  (l/vertically (for [root-call (:root-calls trace)]
                  (call-view root-call))))


(defn create-trace-control [trace-channel]
  (fn [view-context]
    (let [throttled-channel (csp/throttle-at-constant-rate trace-channel 500)]

      (async/go-loop []
        (when-let [new-trace (async/<! throttled-channel)]
          (gui/apply-to-state view-context assoc :trace new-trace)
          (recur))))

    {:local-state {:trace (create-state)}
     :view #'trace-view}))


(defmacro with-trace [& body]
  `(let [input-channel# (async/chan 50)
         trace-channel# (async/chan)]
     (start-tracer input-channel# trace-channel#)
     (.start (Thread. (fn []
                        (gui/start-control (create-trace-control trace-channel#)))))

     #_(async/>!! input-channel# {})
     (debug/with-debug-channel input-channel# ~@body)
     (async/close! input-channel#)))

(run-tests)
(gui/redraw-last-started-view)

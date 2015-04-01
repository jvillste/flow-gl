(ns flow-gl.tools.trace
  (:require [flow-gl.debug :as debug]
            [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [gui :as gui]
                         [cache :as cache]
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
                                     :result (:result entry)))]
          (let [trace (-> trace
                          (update-in [:open-calls] dissoc (:call-id entry))
                          (update-in [:current-calls] assoc (:thread entry) (:parent ending-call)))]
            (if (:parent ending-call)
              (update-in trace [:open-calls (:parent ending-call) :child-calls] conj ending-call)
              (update-in trace [:root-calls] conj ending-call)))))
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

(defn trace-ns* [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core flow-gl.tools.trace} (.name ns))
      (let [ns-fns (namespace-function-vars ns)]
        (doseq [f ns-fns]

          (trace-var* f))))))

(defmacro trace-ns [ns]
  `(trace-ns* ~ns))


(defn  untrace-var*
  ([ns s]
     (untrace-var* (ns-resolve ns s)))
  ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)
           f  ((meta v) ::traced)]
       (when f
         (doto v
           (alter-var-root (constantly ((meta v) ::traced)))
           (alter-meta! dissoc ::traced))))))

(defn untrace-ns* [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
      (untrace-var* f))))

(defmacro untrace-ns [ns]
  `(untrace-ns* ~ns))


(defn traced?
  "Returns true if the given var is currently traced, false otherwise"
  [v]
  (let [^clojure.lang.Var v (if (var? v) v (resolve v))]
    (-> v meta ::traced nil? not)))

(defn start-tracer [input-channel trace-channel]
  (.start (Thread. (fn []
                     (async/go-loop [trace (create-state)]
                       (if-let [entry (async/<! input-channel)]
                         (let [new-trace (add-entry trace entry)]
                           (async/>! trace-channel new-trace)
                           (recur new-trace))
                         (async/close! trace-channel)))))))



;; UI

(defn text-cell [value]
  (l/margin 1 2 1 2 (controls/text value)))

(defn tab-view [view-context state]
  (l/vertically (l/horizontally (for [tab-index (range (count (:tabs state)))]
                                  (let [tab (get (:tabs state)
                                                 tab-index)]
                                    (-> (l/margin 1 2 1 2 (controls/text (:title tab)
                                                                         (if (= (:selected-tab-index state)
                                                                                tab-index)
                                                                           [255 255 255 255]
                                                                           [100 100 100 255])))
                                        (gui/on-mouse-clicked-with-view-context view-context
                                                                                (fn [state event]
                                                                                  (assoc state :selected-tab-index tab-index)))))))
                (:content (get (:tabs state)
                               (:selected-tab-index state)))))

(defn tab [view-context]
  {:local-state {:tabs []
                 :selected-tab-index 0}
   :view #'tab-view})




(defn open-button [view-context state call-id]
  (if ((:open-calls state) call-id)
    (-> (text-cell "-")
        (gui/on-mouse-clicked-with-view-context view-context
                                                (fn [state event]
                                                  (update-in state [:open-calls] disj call-id))))

    (-> (text-cell "+")
        (gui/on-mouse-clicked-with-view-context view-context
                                                (fn [state event]
                                                  (update-in state [:open-calls] conj call-id))))))

(defn value-string [value]
  (cond (number? value)
        (str value)

        (map? value)
        (str "{" (count (keys value)) "}")

        (vector? value)
        (str "[" (count value) "]")

        (function? value)
        "fn"
        
        #_(seq? value)
        #_(str "(" (count value) ")")

        :default
        (str (.getSimpleName (type value)))))

(defn call-view [view-context state call]
  (l/vertically (l/horizontally (when (> (count (:child-calls call)) 0)
                                  (open-button view-context state (:call-id call)))
                                (text-cell (apply str
                                                  (flatten [(count (:child-calls call)) ": "
                                                            "("
                                                            (name (or (:function-symbol call)
                                                                      :x))
                                                            " "
                                                            (interpose " " (for [argument (:arguments call)]
                                                                             (value-string argument)))
                                                            ") -> "
                                                            (value-string (:result call))]))))
                (when ((:open-calls state) (:call-id call))
                  (l/margin 0 0 0 20
                            (l/vertically (for [child (:child-calls call)]
                                            (call-view view-context state child)))))))


(defn trace-view [view-context {:keys [trace] :as state}]
  (l/vertically (-> (controls/text "clear" button-text-color)
                    (gui/on-mouse-clicked-with-view-context view-context
                                                            (fn [state event]
                                                              (async/put! (:input-channel state) {:clear-trace true})
                                                              state)))
                (for [root-call (:root-calls trace)]
                  (when (not ((:hidden-threads state) (:thread root-call)))
                    (l/horizontally (text-cell (:thread root-call))
                                    (call-view view-context state root-call))))))

(defn thread-view [view-context state]
  (layouts/grid (concat [[(l/margin 0 5 0 0 (controls/text "thread"))
                          (controls/text "hidden")]]
                        (for [thread (->> (-> state :trace :root-calls)
                                          (map :thread)
                                          (apply hash-set))]
                          [(controls/text thread) (if ((:hidden-threads state) thread)
                                                    (-> (controls/text "X")
                                                        (gui/on-mouse-clicked-with-view-context view-context
                                                                                                (fn [state event]
                                                                                                  (update-in state [:hidden-threads] disj thread))))
                                                    (-> (controls/text "O")
                                                        (gui/on-mouse-clicked-with-view-context view-context
                                                                                                (fn [state event]
                                                                                                  (update-in state [:hidden-threads] conj thread)))))]))))

(declare trace-root-view)

(def button-text-color [100 255 255 255])
(def header-text-color [200 200 200 255])

(defn functions-view [view-context state]
  (l/vertically (gui/call-and-bind view-context
                                   state
                                   :namespace-filter
                                   :text
                                   controls/text-editor
                                   :namespace-filter)
                (l/horizontally (l/margin 0 10 0 0 (l/vertically (for [namespace  (->> (all-ns)
                                                                     (filter #(if (not (= "" (:namespace-filter state)))
                                                                                (.contains (str (.name %)) (:namespace-filter state))
                                                                                true))
                                                                     (sort-by #(.name %)))]
                                                 (-> (controls/text (.name namespace)
                                                                    (if (= (:selected-namespace state)
                                                                           namespace)
                                                                      [255 255 255 255]
                                                                      [100 100 100 255]))
                                                     (gui/on-mouse-clicked-with-view-context view-context
                                                                                             (fn [state event]
                                                                                               (assoc state :selected-namespace namespace)))))))
                                (when-let [selected-namespace (:selected-namespace state)]

                                  (l/vertically (l/horizontally (l/margin 0 5 0 0 (-> (controls/text "trace all" button-text-color)
                                                                                      (gui/on-mouse-clicked-with-view-context view-context
                                                                                                                              (fn [state event]
                                                                                                                                (trace-ns selected-namespace)
                                                                                                                                (assoc state :refresh-time (.getTime (java.util.Date.)))))))
                                                                (-> (controls/text "untrace all" button-text-color)
                                                                    (gui/on-mouse-clicked-with-view-context view-context
                                                                                                            (fn [state event]
                                                                                                              (untrace-ns selected-namespace)
                                                                                                              (assoc state :refresh-time (.getTime (java.util.Date.)))))))
                                                (layouts/grid (concat [[(l/margin 0 5 0 0 (controls/text "function" header-text-color))
                                                                        (controls/text "traced?" header-text-color)]]
                                                                      (for [function-var (namespace-function-vars selected-namespace)]
                                                                        [(controls/text (-> function-var meta :name))
                                                                         (if (traced? function-var)
                                                                           (-> (controls/text "X")
                                                                               (gui/on-mouse-clicked-with-view-context view-context
                                                                                                                       (fn [state event]
                                                                                                                         (untrace-var* function-var)
                                                                                                                         (assoc state :refresh-time (.getTime (java.util.Date.))))))

                                                                           (-> (controls/text "O")
                                                                               (gui/on-mouse-clicked-with-view-context view-context
                                                                                                                       (fn [state event]
                                                                                                                         (trace-var* function-var)
                                                                                                                         (assoc state :refresh-time (.getTime (java.util.Date.)))))))]))))
                                  ))))


(defn trace-root-view [view-context state]
  (l/preferred (gui/call-view view-context tab :tab {:tabs [{:title "trace"
                                                             :content (trace-view view-context state)}
                                                            {:title "threads"
                                                             :content (thread-view view-context state)}
                                                            {:title "functions"
                                                             :content (functions-view view-context state)}]})))

(defn create-trace-control [input-channel trace-channel]
  (fn [view-context]
    (let [throttled-channel (csp/throttle-at-constant-rate trace-channel 500)]

      (async/go-loop []
        (when-let [new-trace (async/<! throttled-channel)]
          (gui/apply-to-state view-context assoc :trace new-trace)
          (recur))))

    {:local-state {:trace (create-state)
                   :input-channel input-channel
                   :open-calls #{}
                   :hidden-threads #{}
                   :namespace-filter "flow"}
     :view #'trace-root-view}))


(defmacro with-trace [& body]
  `(let [input-channel# (async/chan 50)
         trace-channel# (async/chan)]
     (start-tracer input-channel# trace-channel#)
     (.start (Thread. (fn []
                        (gui/start-control (create-trace-control input-channel# trace-channel#)))))

     #_(async/>!! input-channel# {})
     (debug/with-debug-channel input-channel# ~@body)
     (async/close! input-channel#)))

(run-tests)
(gui/redraw-last-started-view)

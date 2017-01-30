(ns flow-gl.tools.trace
  (:require [flow-gl.debug :as debug]
            [flow-gl.opengl.jogl.opengl :as opengl]
            [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            (fungl [cache :as cache]
                   [application :as application]
                   [atom-registry :as atom-registry]
                   [layouts :as layouts]
                   [layout :as layout])
            (flow-gl.gui [visuals :as visuals]
                         [quad-renderer :as quad-renderer]
                         [tiled-renderer :as tiled-renderer]
                         [animation :as animation]
                         [scene-graph :as scene-graph]
                         [stateful :as stateful]
                         [keyboard :as keyboard]
                         [events :as events]
                         [render-target-renderer :as render-target-renderer])
            (flow-gl.graphics [font :as font]
                              [rectangle :as rectangle])
            #_(flow-gl.gui [drawable :as drawable]
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



;; UI

(def keyword-color [100 255 100 255])
(def button-text-color [100 255 255 255])
(def header-text-color [200 200 200 255])
(def selected-value-color [200 200 0 255])
(def default-color [255 255 255 255])

(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (text value color (font/create "LiberationMono-Regular.ttf" 15)))

  ([value color font]
   (visuals/text color
                 font
                 (str value))))

(defn text-cell
  ([value]
   (text-cell value [255 255 255 255]))

  ([value color]
   (layouts/with-margins 1 2 1 2 (text value color))))


(defn mouse-clicked? [event]
  (= :mouse-clicked
     (:type event)))

(defn set-mouse-clicked-handler [node function]
  (assoc node :mouse-event-handler
         (fn [node event]
           (when (mouse-clicked? event)
             (function))
           event)))

(defmacro when-mouse-clicked [node & body]
  `(set-mouse-clicked-handler ~node (fn [] ~@body)))

(defn open-button [state reduce! call-id child-count]
  (if ((:open-calls state) call-id)
    (assoc (text-cell (str "-" child-count))
           :mouse-event-handler (fn [node event]
                                  (when (mouse-clicked? event)
                                    (reduce! update-in [:open-calls] disj call-id))
                                  event))

    (assoc (text-cell (str "+" child-count))
           :mouse-event-handler (fn [node event]
                                  (when (mouse-clicked? event)
                                    (reduce! update-in [:open-calls] conj call-id))
                                  event))))


(defn map-type-name [map-value]
  (let [type-name (.getSimpleName (type map-value))]
    (if (or (= type-name "PersistentArrayMap")
            (= type-name "PersistentHashMap"))
      ""
      type-name)))

(defn value-string [value]
  (cond (number? value)
        (str value)

        (keyword? value)
        (str value)

        (instance? java.lang.Boolean value)
        (str value)

        (instance? java.lang.Character value)
        (str "Character:" value)

        (string? value)
        (str "\"" value "\"")

        (set? value)
        (str "#{" (count value) "}")

        (instance? clojure.lang.Atom value)
        "(atom)"

        (instance? clojure.lang.Var value)
        (str "(var " (:ns (meta value)) "/" (:name (meta value)) ")")

        (instance? clojure.lang.LazySeq value)
        (str "(LazySeq " (count (take 20 value)) ")")


        (instance? clojure.lang.ArraySeq value)
        (str "(ArraySeq " (count value) ")")

        (seq? value)
        (str "(seq " (count value) ")")

        (map? value)
        (str (map-type-name value) "{" (count (keys value)) "}")

        (instance? clojure.lang.Cons value)
        (str "(cons " (count value) ")")
        
        (vector? value)
        (str "[" (count value) "]")

        (function? value)
        (str value)

        (nil? value)
        "nil"

        (list? value)
        (str "(" (count value) ")")

        (instance? java.lang.Exception value)
        (str (type value))

        :default
        (str (.getSimpleName (type value)))))

(declare value-view)

(defn close-value-on-mouse-click [node reduce! value]
  (set-mouse-clicked-handler node (fn [] (reduce! update-in [:open-values] disj value))))

(defn open-collection [state reduce! open-paren close-paren value]
  (layouts/vertically (-> (text open-paren)
                          (close-value-on-mouse-click reduce! value))
                      (layouts/with-margins 0 0 0 20
                        (layouts/vertically (for [content value]
                                              (value-view state reduce! content false))))

                      (text close-paren)))

(defn open-ref [state reduce! open-paren close-paren value]
  (layouts/vertically (-> (text open-paren)
                          (close-value-on-mouse-click reduce! value))
                      (layouts/with-margins 0 0 0 20
                        (value-view state reduce! @value false))
                      (text close-paren)))

(defn value-view [state reduce! value root-value]
  (let [open-values (:open-values state)]
    (cond (number? value)
          (text-cell (value-string value))

          (fn? value)
          (text-cell (value-string value))
          
          (keyword? value)
          (text (value-string value) keyword-color)

          (string? value)
          (text-cell (value-string value))

          :default
          (if (or root-value
                  (open-values value))
            (cond (map? value)
                  (layouts/vertically (-> (text-cell (str (map-type-name value) "{"))
                                          (close-value-on-mouse-click reduce! value))
                                      (layouts/with-margins 0 0 0 20 (layouts/vertically (for [key (keys value)]
                                                                                           (layouts/horizontally (layouts/with-margins 0 5 0 0  (value-view state reduce! key false))
                                                                                                                 (value-view state reduce! (get value key) false)))))
                                      (text-cell "}"))

                  (vector? value)
                  (open-collection state reduce! "[" "]" value)

                  (instance? clojure.lang.ArraySeq value)
                  (open-collection state reduce! "(ArraySeq" ")" value)

                  (instance? clojure.lang.LazySeq value)
                  (open-collection state reduce! "(lazy-seq" ")" value)

                  (instance? clojure.lang.Cons value)
                  (open-collection state reduce! "(cons" ")" value)

                  (list? value)
                  (open-collection state reduce! "(" ")" value)

                  (seq? value)
                  (open-collection state reduce! "(seq" ")" value)
                  
                  (instance? clojure.lang.Atom value)
                  (open-ref state reduce! "(atom" ")" value)

                  (instance? clojure.lang.Var value)
                  (open-ref state reduce! (str "(var " (:ns (meta #'value-view)) "/" (:name (meta value))) ")" value)

                  (set? value)
                  (open-collection state reduce! "#(" ")" value)

                  (instance? java.lang.Exception value)
                  (text (str (type value) ": " (.getMessage value)))

                  :default (-> (text-cell (str "-" (value-string value)))
                               (close-value-on-mouse-click  reduce! value)))
            
            (assoc (text (value-string value))
                   :mouse-event-handler (fn [node event]
                                          (when (mouse-clicked? event)
                                            (reduce! update-in [:open-values] conj value))
                                          event))))))


(defn initialize-value-view-state []
  {:open-values #{}})

(def value-view-stateful
  {:initialize-state initialize-value-view-state})


(defn call-view [state reduce! call]
  (let [character-width (:width (layout/size (layout/do-layout (text "0"))))]
    (layouts/vertically (layouts/horizontally (layouts/with-minimum-size (* 4 character-width) 0
                                                (if (> (count (:child-calls call)) 0)
                                                  (open-button state reduce! (:call-id call) (count (:child-calls call)))
                                                  nil))

                                              #_(layouts/with-minimum-size (* 5 character-width) 0
                                                  (text-cell (- (:call-ended call)
                                                                (:call-started call))))

                                              (text (str "("
                                                         (if-let [function-symbol (:function-symbol call)]
                                                           (str (name function-symbol) " ")
                                                           "")))
                                              (for [argument (:arguments call)]
                                                (layouts/with-margins 0 0 0 5
                                                  (assoc (text (value-string argument)
                                                               (if (= argument
                                                                      (:selected-value state))
                                                                 selected-value-color
                                                                 default-color))
                                                         :mouse-event-handler (fn [node event]
                                                                                (when (mouse-clicked? event)
                                                                                  (reduce! assoc :selected-value argument))
                                                                                event))))
                                              (text ")")
                                              (when (:function-symbol call)
                                                (assoc (text (str " -> " (value-string (or (:exception call)
                                                                                           (:result call))))
                                                             (if (= (or (:exception call)
                                                                        (:result call))
                                                                    (:selected-value state))
                                                               selected-value-color
                                                               default-color))
                                                       :mouse-event-handler (fn [node event]
                                                                              (when (mouse-clicked? event)
                                                                                (reduce! assoc :selected-value (or (:exception call)
                                                                                                                   (:result call))))
                                                                              event))))
                        (when ((:open-calls state) (:call-id call))
                          (layouts/with-margins 0 0 0 (* 5 character-width)
                            (apply layouts/vertically (for [child (:child-calls call)]
                                                        (call-view state reduce! child))))))))


(defn render-to-texture-render [id scene-graph gl]
  (stateful/with-state-atoms! [quad-renderer-atom [id :render-to-texture-quad-renderer]  (quad-renderer/stateful gl)
                               render-target-renderer-atom [id :render-to-texture-render-target] (render-target-renderer/stateful gl)]
    #_(println "previous " id @quad-renderer-atom #_@render-target-renderer-atom #_(:previous-scene-graph @render-target-renderer-atom))
    (render-target-renderer/render render-target-renderer-atom gl scene-graph
                                   (fn []
                                     (opengl/clear gl 0 0 0 0)
                                     (quad-renderer/render quad-renderer-atom gl (assoc scene-graph
                                                                                        :x 0 :y 0))))))
(defn render-to-texture [node id]
  (assoc node
         :render [render-to-texture-render id]))


(defn trace-view [state reduce!]
  (let [last-call-started (:call-started (last (:root-calls (:trace state))))]
    (-> (layouts/vertically (for [root-call (:root-calls (:trace state))]
                              (do #_(prn root-call)
                                  (layouts/box 2 (visuals/rectangle [0 150 0 (max 0
                                                                                  (- 255
                                                                                     (* 255
                                                                                        (/ (- last-call-started
                                                                                              (:call-started root-call))
                                                                                           5000))))]
                                                                    0 0)
                                               (cache/call! call-view state reduce! root-call)))))
        #_(render-to-texture :trace-view))))


(defn do-layout [scene-graph width height]
  (-> scene-graph
      (assoc :x 0
             :y 0
             :available-width width
             :available-height height)
      (layout/do-layout)))

(defn create-trace-scene-graph [state reduce! width height]
  #_(println "create-trace-scene-graph")
  (animation/swap-state! animation/set-wake-up 1000)
  (-> (layouts/vertically
       (cache/call! trace-view state reduce!)
       (assoc (visuals/rectangle [255 255 255 255] 0 0)
              :height 5)
       (value-view (stateful/stateful-state! :value-view value-view-stateful)
                   (stateful/reducer! :value-view)
                   (:selected-value state)
                   true)
       #_(stateful/with-state-atoms! [value-view-state-atom :value-view value-view-stateful]
           (value-view value-view-state-atom
                       (:selected-value @state-atom)
                       true)))
      (assoc :x 0
             :y 0
             :available-width width
             :available-height height)
      (layout/do-layout)))

(defn trace-printer-stateful [trace-channel reduce!]
  (let [throttled-channel (csp/throttle-at-constant-rate trace-channel 500)]
    (async/go-loop []
      (when-let [new-trace (async/<! trace-channel)]
        (reduce! assoc :trace
                 new-trace)
        
        (animation/swap-state! animation/set-wake-up 0)
        #_(async/put! event-channel {:type :request-redraw})
        (recur))))
  
  {:initialize-state (fn []
                       {:trace (create-state)
                        :open-calls #{}})})

(def trace-printer-atom-specification
  {:create (fn []
             {:trace (create-state)
              :open-calls #{}})})

(defn create-trace-printer [id trace-channel]
  (let [state-atom  (atom-registry/get! id trace-printer-atom-specification)
        throttled-channel (csp/throttle-at-constant-rate trace-channel 500)]
    (async/go-loop []
      (when-let [new-trace (async/<! throttled-channel)]
        (swap! state-atom
               assoc :trace
               new-trace)
        (animation/swap-state! animation/set-wake-up 0)
        #_(async/put! event-channel {:type :request-redraw})
        (recur)))
    state-atom))

(defn reducer-for-atom [reduced-atom]
  (fn [function & arguments]
    (apply swap! reduced-atom function arguments)))


(defn start-trace-printer [trace-channel]
  #_(create-trace-printer :trace-printer trace-channel)
  (let [event-channel (application/start-window (fn [width height]
                                                  (let [trace-printer-state-atom (cache/call! create-trace-printer :trace-printer trace-channel)
                                                        reduce! (cache/call! reducer-for-atom trace-printer-state-atom)]
                                                    (#'create-trace-scene-graph
                                                     @trace-printer-state-atom
                                                     reduce!
                                                     width
                                                     height))))]))

(defmacro with-trace [& body]
  `(let [input-channel# (async/chan 50)
         trace-channel# (async/chan)]
     (start-tracer input-channel# trace-channel#)
     #_(gui/start-control (create-trace-control input-channel# trace-channel#))
     (start-trace-printer trace-channel#)
     (debug/with-debug-channel input-channel# ~@body)
     #_(Thread/sleep 1000)
     #_(async/close! input-channel#)))

#_(defn start []
    #_(application/start-window (partial create-trace-scene-graph
                                         (atom (create-state))))

    (let [value-atom (atom :a)]
      (application/start-window (fn [width height]
                                  (let [[state reduce!] (stateful/state-and-reducer! :value-view value-view-stateful)]
                                    (-> (value-view state
                                                    reduce!
                                                    {:string "haa"
                                                     :map {:a {:b {:c :d}}}
                                                     :vector [1 2 3]
                                                     :list '(1 2 3)
                                                     :lazy (for [i (range 3)]
                                                             i)
                                                     :cons (cons :a (cons :b [:c]))
                                                     :atom value-atom
                                                     :var (var value-view )
                                                     :fn (fn [])}
                                                    true)
                                        (do-layout width height)))))))

(defn foo [x] {:x x})
(defn bar [x] (foo (+ 1 x)))

(defn start []
  (trace-var #'foo)
  (trace-var #'bar)
  #_(with-trace
      (bar 10))
  (with-trace
    (log {:foo [:bar :baz]}
         {:foo [:foobar :foobaz]})))

#_(defn inspect-value [value]
    (async/thread (gui/start-app (gui/control-to-application value-inspector {:value value}))))

#_(run-tests)


#_(with-trace
    (log {:foo [:bar :baz]}))


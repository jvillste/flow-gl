(ns flow-gl.tools.trace-ui
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [flow-gl.csp :as csp]
            [flow-gl.debug :as debug]
            [flow-gl.graphics.font :as font]
            [flow-gl.gui.animation :as animation]
            [flow-gl.gui.stateful :as stateful]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.atom-registry :as atom-registry]
            [fungl.cache :as cache]
            [fungl.layout :as layout]
            [fungl.layouts :as layouts]
            [fungl.layout.measuring :as measuring]
            [flow-gl.tools.trace :as trace]))

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
   (text value color (font/create (.getPath (io/resource "LiberationSans-Regular.ttf")) 15)))

  ([value color font]
   (visuals/text (str value)
                 color
                 35 #_font
                )))

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
                  (layouts/vertically (-> (text-cell (str (map-type-name value) " {"))
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
  (let [character-width (:width (measuring/size (layout/do-layout (text "0"))))]
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




(defn trace-view [state reduce!]
  (let [last-call-started (:call-started (last (:root-calls (:trace state))))]
    (layouts/vertically (for [root-call (:root-calls (:trace state))]
                          (do #_(prn root-call)
                              (layouts/box 2 (visuals/rectangle [0 50 0 (max 0
                                                                             (- 255
                                                                                (* 255
                                                                                   (/ (- last-call-started
                                                                                         (:call-started root-call))
                                                                                      5000))))]
                                                                0 0)
                                           (cache/call! call-view state reduce! root-call)))))))


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
                       {:trace (trace/create-state)
                        :open-calls #{}})})

(def trace-printer-atom-specification
  {:create (fn []
             {:trace (trace/create-state)
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

(defn show-value [value]
  (let [value-atom (atom :a)]
      (application/start-window (fn [width height]
                                  (let [[state reduce!] (stateful/state-and-reducer! :value-view value-view-stateful)]
                                    (-> (value-view state
                                                    reduce!
                                                    value
                                                    true)
                                        (do-layout width height))))))
  nil)

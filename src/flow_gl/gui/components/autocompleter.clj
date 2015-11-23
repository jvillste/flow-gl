(ns flow-gl.gui.components.autocompleter
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            [flow-gl.tools.trace :as trace]
            [flow-gl.csp :as csp]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view]
                         [gui :as gui]
                         [controls :as controls])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
            [flow-gl.gui.layout-dsl :as l]
            [clj-http.client :as client])
  (:use flow-gl.utils
        midje.sweet
        
        clojure.test))




;; CSP

(defn tap-new
  ([mult]
   (tap-new mult (async/chan)))

  ([mult channel]
   (async/tap mult channel)
   channel))

(defn throttle [input-channel interval]
  (let [mult (async/mult input-channel)
        throttled-channel (async/chan)
        unthrottled-channel (tap-new mult)
        unthrottled-channel-2 (tap-new mult)]

    (async/go-loop [value (async/<! unthrottled-channel-2)]
      (when value
        (async/alt! (async/timeout interval) (do (>! throttled-channel value)
                                                 (recur (async/<! unthrottled-channel-2)))
                    unthrottled-channel-2 ([value] (recur value)))))
    [throttled-channel unthrottled-channel]))

#_(let [source (async/chan)
        [throttled unthrottled] (throttle source 1000)
        control (async/timeout 10000)]
    (async/go (dotimes [n 10]
                (if (= n 5)
                  (async/<! (async/timeout 1200))
                  (async/<! (async/timeout 200)))

                (println "sending" n)
                (async/>! source n)))
    (async/go (loop []
                (async/alt! control (println "exiting")
                            throttled ([value]
                                       (println "got" value)
                                       (recur))
                            unthrottled ([value]
                                         #_(println "got from unthrottled" value)
                                         (recur))))))


;; Text editor

(defn handle-new-text [state new-text]
  ((:on-change state) new-text)
  state)

(defn handle-text-editor-event [state event]
  (trace/log "got in the editor" event)
  (cond
    (events/key-pressed? event :back-space)
    (handle-new-text state (apply str (drop-last (:text state))))

    (and (:character event)
         (not= (:key event)
               :enter)
         (= (:type event)
            :key-pressed))
    (handle-new-text state (str (:text state)
                                (:character event)))

    :default
    state))


(defn text-editor-view [view-context state]
  (l/box 10
         (drawable/->Rectangle 0
                               0
                               (cond
                                 (:has-focus state) [0 200 200 255]
                                 (:mouse-over state) [0 180 180 255]
                                 :default [0 120 120 255]))
         (drawable/->Text (str (:text state))
                          (font/create "LiberationSans-Regular.ttf" 15)
                          (if (:has-focus state)
                            [0 0 0 255]
                            [100 100 100 255]))))


(defn text-editor [view-context]
  {:local-state {:text ""}
   :view #'text-editor-view
   :handle-keyboard-event (fn [state event]
                            (gui/apply-to-local-state state view-context handle-text-editor-event event))
   :can-gain-focus true})


;; Auto completer

(defn select [local-state value]
  ((:on-select local-state) value)
  
  (assoc local-state
         :query nil
         :selection nil
         :results []))

(defn auto-completer-view [view-context state]
  (l/vertically (gui/call-view text-editor
                               :query-editor
                               {:text (or (:query state)
                                          ((:text-function state)
                                           (:selected-value state)))
                                :on-change (fn [new-query]
                                             (async/>!! (:query-channel state)
                                                        [(:query-function state)
                                                         new-query]))
                                :on-focus-lost (fn [text-editor-state]
                                                 (gui/send-local-state-transformation view-context assoc
                                                                                      :selection nil
                                                                                      :results [])
                                                 text-editor-state)})

                (layouts/->BottomOutOfLayout [(assoc (l/vertically (doall (map-indexed (fn [index result]
                                                                                         (-> (l/box 10
                                                                                                    (drawable/->Rectangle 0
                                                                                                                          0
                                                                                                                          (if (= index (:mouse-over-index state))
                                                                                                                            (map #(* 255 %) [0 0.8 0.8 1])
                                                                                                                            (if (= (:selection state)
                                                                                                                                   index)
                                                                                                                              (map #(* 255 %) [0 0.8 0.8 1])
                                                                                                                              (map #(* 255 %) [0 0.5 0.5 1]))))
                                                                                                    (drawable/->Text (str ((:text-function state) result))
                                                                                                                     (font/create "LiberationSans-Regular.ttf" 15)
                                                                                                                     [0 0 0 255]))
                                                                                             
                                                                                             (gui/on-mouse-event-with-view-context :mouse-clicked view-context
                                                                                                                                   (fn [state event]
                                                                                                                                     (select state (get (:results state)
                                                                                                                                                        index))))

                                                                                             (gui/on-mouse-event-with-view-context :on-mouse-enter view-context
                                                                                                                                   (fn [state event]
                                                                                                                                     (assoc state
                                                                                                                                            :mouse-over-index index)))))

                                                                                       (:results state))))
                                                     :on-mouse-leave (fn [state]
                                                                       (println "mouse leave")
                                                                       (assoc state
                                                                              :mouse-over-index -1)))])))




(defn autocompleter [view-context throttle]
  (let [query-channel (async/chan)
        state (conj {:local-state {:query nil
                                   :query-function nil
                                   :on-select nil
                                   :text-function identity
                                   :results []
                                   :selected-value ""
                                   :selection nil
                                   :query-channel query-channel}
                     :view #'auto-completer-view
                     :handle-keyboard-event (fn [state event]
                                              (gui/apply-to-local-state state view-context
                                                                        (fn [{:keys [results selection] :as local-state}]
                                                                          (events/on-key local-state event
                                                                                         :down (if (not (empty? results))
                                                                                                 (update-in local-state [:selection]
                                                                                                            (fn [selection]
                                                                                                              (min (if selection
                                                                                                                     (inc selection)
                                                                                                                     0)
                                                                                                                   (dec (count results))))))
                                                                                         
                                                                                         :up (if (not (empty? results))
                                                                                               (update-in local-state [:selection]
                                                                                                          (fn [selection]
                                                                                                            (max 0
                                                                                                                 (if selection
                                                                                                                   (dec selection)
                                                                                                                   0)))))
                                                                                         
                                                                                         :enter (let [value (if (:selection local-state)
                                                                                                              (get (:results local-state)
                                                                                                                   (:selection local-state))
                                                                                                              (:query local-state))]
                                                                                                  (select local-state value)

                                                                                                  )))))}
                    gui/child-focus-handlers)]

    
    (async/go (let [[throttled-query unthrottled-query] (csp/throttle query-channel throttle)]
                (loop []
                  (async/alt! (:control-channel view-context) ([_] (println "exiting auto completer process"))
                              throttled-query ([[query-function query]]
                                               (async/go (let [results (async/<! (async/thread (vec (query-function query))))]
                                                           (gui/apply-to-state view-context
                                                                               assoc
                                                                               :results results
                                                                               :selection nil)))
                                               (recur))

                              unthrottled-query ([[query-function query]]
                                                 (gui/apply-to-state view-context
                                                                     assoc
                                                                     :query query)

                                                 (recur))))))
    state))




;; Test view

(defn query-text [possible-values query]
  (if (= query "")
    []
    (filter #(.contains (:text %) query)
            possible-values)))

(defn root-view [view-context state]
  (l/vertically (gui/call-view autocompleter :completer-1
                               {:query-function (partial query-text (:possible-values state))
                                :selected-value (:selected-value state)
                                :on-select (fn [selection]
                                             (let [value (if (string? selection)
                                                           (or (first (filter #(= (:text %) selection)
                                                                              (:possible-values state)))
                                                               {:text selection :id (int (rand 100))})
                                                           selection)]

                                               (when (not (some #(= (:text %) selection)
                                                                (:possible-values state)))
                                                 (gui/send-local-state-transformation view-context
                                                                                      update-in
                                                                                      [:possible-values]
                                                                                      conj
                                                                                      value))

                                               (gui/send-local-state-transformation view-context
                                                                                    assoc
                                                                                    :selected-value
                                                                                    value)))}
                               [:text
                                0])
                (controls/text (str "selected: " (:selected-value state)))
                (for [value (:possible-values state)]
                  (controls/text value))))

(defn root [view-context]
  (conj {:local-state {:selected-value nil
                       :possible-values #{}}
         :view #'root-view}
        gui/child-focus-handlers))


#_(trace/trace-var 'flow-gl.gui.gui/apply-to-state)
#_(trace/trace-var 'flow-gl.gui.components.autocompleter/autocompleter)
#_(trace/untrace-ns 'flow-gl.gui.components.autocompleter)
#_(trace/trace-ns 'flow-gl.gui.components.autocompleter)


(defn start []
  #_(gui/start-control root)

  (trace/with-trace
    (gui/start-control root)))


#_(run-tests)


;; query channel
;; 

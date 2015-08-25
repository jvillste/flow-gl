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
  (when (:on-change state)
    (async/go (async/>! (:on-change state) new-text)))
  (assoc-in state [:text] new-text))

(defn handle-text-editor-event [state event]
  (cond
    (events/key-pressed? event :back-space)
    (handle-new-text state (apply str (drop-last (:text state))))

    (and (:character event)
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
         (drawable/->Text (:text state)
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

(defn query-wikipedia [query]
  (let [channel (async/chan)]
    (async/put! channel (second (:body (client/get (str "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=" query) {:as :json}))))
    channel))

(defn auto-completer-view [view-context state]
  (l/vertically (gui/call-view text-editor
                               :query-editor
                               {:text (:query state)
                                :on-change (:query-channel state)
                                :on-focus-lost (fn [text-editor-state]
                                                 (println "focus lost")
                                                 (async/put! (:selection-channel state) :cancel)
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
                                                                                                    (drawable/->Text result
                                                                                                                     (font/create "LiberationSans-Regular.ttf" 15)
                                                                                                                     [0 0 0 255]))
                                                                                             (gui/on-mouse-event-with-view-context :mouse-clicked view-context
                                                                                                                                   (fn [state event]
                                                                                                                                     (trace/log "mouse selection by clck" index state event)
                                                                                                                                     (async/>!! (:selection-channel state) index)
                                                                                                                                     state))

                                                                                             (gui/on-mouse-event-with-view-context :on-mouse-enter view-context
                                                                                                                                   (fn [state event]
                                                                                                                                     (assoc state
                                                                                                                                            :mouse-over-index index)))))

                                                                                       (:results state))))
                                                     :on-mouse-leave (fn [state]
                                                                       (println "mouse leave")
                                                                       (assoc state
                                                                              :mouse-over-index -1)))])))


(defn autocompleter [view-context query-function]
  (let [query-channel (async/chan)
        selection-channel (async/chan)
        trace-channel @flow-gl.debug/debug-channel
        state (conj {:local-state {:query ""
                                   :results []
                                   :selection nil
                                   :selection-channel selection-channel
                                   :query-channel query-channel}
                     :view #'auto-completer-view
                     :handle-keyboard-event (fn [state event]
                                              (events/on-key state event
                                                             :down (do (println "got down")
                                                                       (async/put! selection-channel :next)
                                                                       state)
                                                             :up (do (async/put! selection-channel :previous)
                                                                     state)
                                                             :enter (do
                                                                      (let [local-state (gui/get-local-state state view-context)]
                                                                        (if (:selection local-state)
                                                                          (do (println "sending selection" (:selection local-state) (:results local-state))
                                                                              (async/put! selection-channel :select)
                                                                              (assoc state
                                                                                     :stop-keyboard-event-handling true))
                                                                          state)))))}
                    gui/child-focus-handlers)]

    
    (async/go (let [[throttled-query unthrottled-query] (csp/throttle query-channel 500)]
                (loop []
                  (async/alt! (:control-channel view-context) ([_] (println "exiting auto completer process"))
                              throttled-query ([query]
                                               (async/go (let [results (async/<! (async/thread (vec (query-function query))))]
                                                           (gui/apply-to-state view-context
                                                                               assoc
                                                                               :results results
                                                                               :selection nil)))
                                               (recur))

                              unthrottled-query ([query]
                                                 (gui/apply-to-global-state view-context
                                                                            gui/update-binding
                                                                            view-context
                                                                            (fn [old-query]
                                                                              query)
                                                                            :query)
                                                 (recur))))))

    (async/go-loop [] (async/alt! (:control-channel view-context) ([_] (println "exiting selection process"))
                                  selection-channel ([selection-event]
                                                     (case selection-event
                                                       :next (do (trace/log "apply-to-state on next")
                                                                 (println "apply-to-state on next")
                                                                 (gui/apply-to-state view-context
                                                                                     (fn [{:keys [results selection] :as state}]
                                                                                       (if (not (empty? results))
                                                                                         (update-in state [:selection]
                                                                                                    (fn [selection]
                                                                                                      (min (if selection
                                                                                                             (inc selection)
                                                                                                             0)
                                                                                                           (dec (count results)))))))))
                                                       :select (gui/apply-to-state view-context
                                                                                   (fn [{:keys [results selection] :as state}]
                                                                                     (println "selected" results selection)
                                                                                     (gui/apply-to-global-state view-context
                                                                                                                gui/update-binding
                                                                                                                view-context
                                                                                                                (fn [old-query]
                                                                                                                  (get results selection))
                                                                                                                :query)
                                                                                     (assoc state
                                                                                            :selection nil
                                                                                            :results []
                                                                                            :query (get results selection))))
                                                       
                                                       
                                                       :cancel (gui/apply-to-state view-context
                                                                                   assoc
                                                                                   :selection nil
                                                                                   :results [])
                                                       
                                                       (when (number? selection-event)
                                                         (gui/apply-to-state view-context
                                                                             (fn [{:keys [results] :as state}]
                                                                               (trace/log-to-channel trace-channel "got selected index" selection-event state)
                                                                               (gui/apply-to-global-state view-context
                                                                                                          gui/update-binding
                                                                                                          view-context
                                                                                                          (fn [old-query]
                                                                                                            (get results selection-event))
                                                                                                          :query)
                                                                               (assoc state
                                                                                      :selection nil
                                                                                      :results [])))))


                                                     (recur))))
    state))




;; Test view

(defn query-text [query]
  (Thread/sleep 1000)
  ["a" "b" "c" query])

(defn root-view [view-context state]
  (l/vertically (gui/call-and-bind view-context state :text-1 :query
                                   autocompleter :completer-1
                                   {}
                                   [query-text])))

(defn root [view-context]
  (conj {:local-state {:text-1 "a"
                       :text-2 "b"}
         :view #'root-view}
        gui/child-focus-handlers))


(defn start []
  #_(.start (Thread. (fn []
                       (gui/start-control root))))

  (.start (Thread. (fn []
                     (trace/with-trace
                       (trace/trace-var 'flow-gl.gui.gui/apply-to-state)
                       #_(trace/trace-var 'flow-gl.gui.components.autocompleter/autocompleter)
                       (trace/untrace-ns 'flow-gl.gui.components.autocompleter)
                       (trace/trace-ns 'flow-gl.gui.components.autocompleter)
                       (gui/start-control root))))))


#_(run-tests)

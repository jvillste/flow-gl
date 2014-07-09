(ns examples.autocompleter
  (:require [clojure.core.async :as async]
            [flow-gl.tools.layoutable-inspector :as layoutable-inspector]
            [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events]
                         [quad-view :as quad-view]
                         [quad-gui :as quad-gui])

            (flow-gl.graphics [command :as command]
                              [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]
                                 [quad-batch :as quad-batch])
            [clj-http.client :as client])
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl
        clojure.test))

(defn query-wikipedia [query]
  (let [channel (async/chan)]
    (async/put! channel (second (:body (client/get (str "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=" query) {:as :json}))))
    channel))


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
   [(handle-new-text state (apply str (drop-last (:text state))))
    false]

   (and (:character event)
        (= (:type event)
           :key-pressed))
   [(handle-new-text state (str (:text state)
                                (:character event)))
    false]

   :default
   [state true]))

(defn crate-text-editor [state-path event-channel control-channel]
  {:text ""
   :has-focus false
   :handle-keyboard-event handle-text-editor-event})

(defn text-editor-view [state]
  [state
   (layout/->Box 10 [(drawable/->Rectangle 0
                                           0
                                           (if (:has-focus state)
                                             [0 0.8 0.8 1]
                                             [0 0.5 0.5 1]))
                     (drawable/->Text (:text state)
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      (if (:has-focus state)
                                        [0 0 0 1]
                                        [0.3 0.3 0.3 1]))])])

(def text-editor {:constructor crate-text-editor
                  :view text-editor-view})


;; Auto completer

(quad-gui/def-view auto-completer-view [state]
  (layout/->VerticalStack [(quad-gui/call-view :query-editor
                                               text-editor
                                               {:text (:query state)
                                                :on-change (:query-channel state)
                                                :on-focus-lost (fn [text-editor-state]
                                                                 (async/put! (:selection-channel state) :cancel)
                                                                 text-editor-state)})

                           (layout/->AboveBelow [(layout/->VerticalStack (doall (map-indexed (fn [index result]
                                                                                               (-> (layout/->Box 10 [(drawable/->Rectangle 0
                                                                                                                                           0
                                                                                                                                           (if (= (:selection state)
                                                                                                                                                  index)
                                                                                                                                             [0 0.8 0.8 1]
                                                                                                                                             [0 0.5 0.5 1]))
                                                                                                                     (drawable/->Text result
                                                                                                                                      (font/create "LiberationSans-Regular.ttf" 15)
                                                                                                                                      [0 0 0 1])])
                                                                                                   (quad-gui/on-mouse-clicked (fn [state]
                                                                                                                                (async/>!! (:selection-channel state) index)
                                                                                                                                state))))

                                                                                             (:results state))))])]))

(defn create-auto-completer [state-path event-channel control-channel]
  (let [state (conj {:query ""
                     :results []
                     :selection nil
                     :selection-channel (async/chan)
                     :query-channel (async/chan)
                     :handle-keyboard-event (fn [state event]
                                              (events/on-key state event
                                                             :down (do (async/put! (:selection-channel state) :next)
                                                                       state)
                                                             :up (do (async/put! (:selection-channel state) :previous)
                                                                     state)
                                                             :enter (if (:selection state)
                                                                      (do (async/put! (:selection-channel state) :select)
                                                                          state)
                                                                      state)))}
                    quad-gui/child-focus-handlers)]

    (async/go (let [[throttled-query unthrottled-query] (throttle (:query-channel state) 500)]
                (loop []
                  (async/alt! control-channel ([_] (println "exiting auto completer process"))
                              throttled-query ([query]
                                                 (async/go (let [results (async/<! (query-wikipedia query))]
                                                             (quad-gui/transact state-path event-channel
                                                                                (fn [state]
                                                                                  (assoc state
                                                                                    :results results
                                                                                    :selection nil)))))
                                                 (recur))

                              unthrottled-query ([query]
                                                   (quad-gui/transact state-path event-channel
                                                                      (fn [state]
                                                                        (when-let [on-selection (:on-selection state)]
                                                                          (on-selection query))
                                                                        (assoc state :query query)))
                                                   (recur))))))

    (async/go-loop [] (async/alt! control-channel ([_] (println "exiting selection process"))
                                  (:selection-channel state) ([selection-event]
                                                                (case selection-event
                                                                  :next (quad-gui/transact state-path event-channel
                                                                                           (fn [{:keys [results selection] :as state}]
                                                                                             (if (not (empty? results))
                                                                                               (update-in state [:selection]
                                                                                                          (fn [selection]
                                                                                                            (min (if selection
                                                                                                                   (inc selection)
                                                                                                                   0)
                                                                                                                 (dec (count results))))))))
                                                                  :select (quad-gui/transact state-path event-channel
                                                                                             (fn [{:keys [results selection] :as state}]
                                                                                               (when-let [on-selection (:on-selection state)]
                                                                                                 (on-selection (get results selection)))
                                                                                               (assoc state
                                                                                                 :selection nil
                                                                                                 :results []
                                                                                                 :query (get results selection))))
                                                                  :cancel (quad-gui/transact state-path event-channel
                                                                                             (assoc state
                                                                                               :selection nil
                                                                                               :results []))
                                                                  (when (number? selection-event)
                                                                    (quad-gui/transact state-path event-channel
                                                                                       (fn [{:keys [results] :as state}]
                                                                                         (when-let [on-selection (:on-selection state)]
                                                                                           (on-selection (get results selection-event)))
                                                                                         (assoc state
                                                                                           :selection nil
                                                                                           :results []
                                                                                           :query (get results selection-event))))))


                                                                (recur))))
    state))


(def auto-completer
  {:constructor create-auto-completer
   :view auto-completer-view})

(quad-gui/def-view view [state]
  (layout/->VerticalStack [(quad-gui/call-view :completer-1
                                               auto-completer
                                               {:query (:text-1 state)
                                                :on-selection (quad-gui/apply-to-current-state [state new-text]
                                                                                               (assoc state :text-1 new-text))})
                           (quad-gui/call-view :completer-2
                                               auto-completer
                                               {:query (:text-2 state)
                                                :on-selection (quad-gui/apply-to-current-state [state new-text]
                                                                                               (assoc state :text-2 new-text))})]))

(defn create [state-path event-channel control-channel]
  (conj {:text-1 ""
         :text-2 ""
         :handle-keyboard-event (fn [state event]
                                  (events/on-key state event
                                                 :esc (do (quad-gui/request-close event-channel)
                                                          state)))}
        quad-gui/child-focus-handlers))

(def root
  {:constructor create
   :view view})

(defn start []
  (quad-gui/start-view root))


(run-tests)

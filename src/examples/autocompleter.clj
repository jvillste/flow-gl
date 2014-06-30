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
                                 [quad-batch :as quad-batch]))
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl
        clojure.test))


(defn handle-text-editor-event [state event]
  (cond

   (events/key-pressed? event :back-space)
   (let [new-text (apply str (drop-last (:text state)))]
     (when (:on-change state)
       ((:on-change state) new-text))
     (assoc-in state [:text] new-text))

   (and (:character event)
        (= (:type event)
           :key-pressed))
   (let [new-text (str (:text state)
                       (:character event))]
     (when (:on-change state)
       ((:on-change state) new-text))
     (assoc-in state [:text] new-text))

   :default
   state))

(def initial-text-editor-state
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

(def text-editor {:initial-state initial-text-editor-state
                  :view text-editor-view})

(quad-gui/def-view view [state]
  (layout/->VerticalStack (concat [(quad-gui/call-view :query-editor
                                                       text-editor
                                                       {:text (:query state)
                                                        :on-change (quad-gui/with-view-context [state-path event-channel new-text]
                                                                     (quad-gui/transact state-path event-channel
                                                                                        (fn [state]
                                                                                          (assoc-in state [:query] new-text)))
                                                                     (async/go (async/<! (async/timeout 1000))
                                                                               (quad-gui/transact state-path event-channel
                                                                                                  (fn [state]
                                                                                                    (assoc-in state [:results] [new-text])))))})]
                                  (for-all [result (:results state)]
                                           (layout/->Box 10 [(drawable/->Rectangle 0
                                                                                   0
                                                                                   [0 0.5 0.5 1])
                                                             (drawable/->Text result
                                                                              (font/create "LiberationSans-Regular.ttf" 15)
                                                                              [0 0 0 1])])))))

#_(quad-gui/apply-to-current-state [state new-text] (assoc-in state [:rows row-index column-index] new-text))

#_(layoutable-inspector/show-layoutable (second (grid-view {:rows [["11" "12"] ["21" "22"]]})))

(def initial-state (conj {:query ""
                          :results ["foo" "bar"]
                          :control-channel (async/chan)
                          :query-channel (async/chan)
                          :handle-keyboard-event (fn [state event]
                                                   (cond
                                                    (events/key-pressed? event :esc)
                                                    (assoc state :close-requested true)

                                                    :default
                                                    state))}
                         quad-gui/child-focus-handlers))


#_(defn run-view-and-layout [state name window quad-view]
    (let [width (window/width window)
          height (window/height window)
          [state layoutable] (utils/named-time (str name " View") (grid-view state))
          layout (utils/named-time (str name " Layout") (layout/layout layoutable width height))
          quad-view (window/with-gl window gl (utils/named-time (str name " Draw") (quad-view/draw-layout quad-view layout width height gl)))]
      [state quad-view]))

#_(let [window (window/create 600
                              600
                              :profile :gl3)
        quad-view (window/with-gl window gl (quad-view/create gl))

        [state quad-view] (run-view-and-layout initial-grid-view-state "1" window quad-view)

        state (update-in state [:rows 5 5] str "a")

        [state quad-view] (run-view-and-layout state "1" window quad-view)]
    (window/close window))


#_(let [c1 (async/chan)
        c2 (async/chan)]
    (async/thread (while true
                    (let [[v ch] (async/alts!! [c1 c2])]
                      (println "Read" v "from" ch))))
    (async/>!! c1 "hi")
    (async/>!! c2 "there"))

(defn start []
  (quad-gui/start-view initial-state
                       view))


(run-tests)


#_(def channel (async/chan))

#_(async/go (async/onto-chan channel [1 2 3 4 5 6 7] false))

#_(async/go-loop [[v ch] (async/alts! [channel (async/timeout 0)] :priority true)]
                 (when (and v (= ch channel))
                   (println "Read" v))
                 (if v (recur (async/alts! [channel (async/timeout 0)] :priority true))))

                                        ;(.start (Thread. go-loop))

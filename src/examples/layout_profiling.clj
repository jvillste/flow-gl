(ns examples.layout-profiling
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

(def event-queue (atom (event-queue/create)))

(defn handle-text-editor-event [state event]
  (if (:editing? state)
    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :text (:edited-text state))
         (assoc :editing? false))

     (events/key-pressed? event :back-space)
     (let [new-text (apply str (drop-last (:edited-text state)))]
       (when (:on-change state)
         ((:on-change state) new-text))
       (assoc-in state [:edited-text] new-text))

     (and (:character event)
          (= (:type event)
             :key-pressed))
     (let [new-text (str (:edited-text state)
                         (:character event))]
       (when (:on-change state)
         ((:on-change state) new-text))
       (assoc-in state [:edited-text] new-text))

     :default
     state)

    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :edited-text (:text state))
         (assoc :editing? true))


     :default
     state)))

(def initial-text-editor-state
  {:text ""
   :edited-text ""
   :editing? false
   :has-focus false
   :handle-keyboard-event handle-text-editor-event})

(defn text-editor-view [state]
  [state
   (layout/->Box 10 [(drawable/->Rectangle 0
                                           0
                                           (if (:has-focus state)
                                             [0 0.8 0.8 1]
                                             [0 0.5 0.5 1]))
                     (drawable/->Text (if (:editing? state)
                                        (:edited-text state)
                                        (:text state))
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      (if (:has-focus state)
                                        (if (:editing? state)
                                          [0 0 1 1]
                                          [0 0 0 1])
                                        [0.3 0.3 0.3 1]))])])

(def text-editor {:initial-state initial-text-editor-state
                  :view text-editor-view})

(quad-gui/def-view grid-view [state]
  (layout/grid (for-all [[row-index cell-texts] (indexed (:rows state))]
                        (for-all [[column-index cell-text] (indexed cell-texts)]
                                 (quad-gui/call-view [:todo-text-editor row-index column-index]
                                                     text-editor
                                                     {:text cell-text
                                                      :on-change (quad-gui/apply-to-current-state [state new-text]
                                                                                                  (assoc-in state [:rows row-index column-index] new-text))})))))


#_(layoutable-inspector/show-layoutable (second (grid-view {:rows [["11" "12"] ["21" "22"]]})))

(def initial-grid-view-state (conj {:rows (vec(for [x (range 10)]
                                                (vec (for [y (range 10)]
                                                       (str x y)))))
                                    :handle-keyboard-event (fn [state event]
                                                             (cond
                                                              (events/key-pressed? event :esc)
                                                              (assoc state :close-requested true)

                                                              :default
                                                              state))}
                                   quad-gui/child-focus-handlers))
(defn run-view-and-layout [state name window quad-view]
  (let [width (window/width window)
        height (window/height window)
        [state layoutable] (utils/named-time (str name " View") (grid-view state))
        layout (utils/named-time (str name " Layout") (layout/layout layoutable width height))
        quad-view (window/with-gl window gl (utils/named-time (str name " Draw") (quad-view/draw-layout quad-view layout width height gl)))]
    [state quad-view]))

(let [window (window/create 600
                            600
                            :profile :gl3)
      quad-view (window/with-gl window gl (quad-view/create gl))

      [state quad-view] (run-view-and-layout initial-grid-view-state "1" window quad-view)

      state (update-in state [:rows 5 5] str "a")

      [state quad-view] (run-view-and-layout state "1" window quad-view)]
  (window/close window))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (quad-gui/start-view @event-queue
                                               initial-grid-view-state
                                               grid-view)))))

(event-queue/add-event @event-queue {})

(run-tests)

(ns examples.scrolling
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font]))
  (:use flow-gl.utils
        clojure.test))

#_(layout/deflayout-not-memoized HorizontalSplit [left-width children]
    (layout [this requested-width requested-height]
            (let [[left middle right] children]
              (assoc this
                :children
                (let [middle-size (layoutable/preferred-size middle requested-width requested-height)
                      left-width (max 0
                                      (min (- requested-width
                                              (:width middle-size))
                                           left-width))]
                  [(layout/set-dimensions-and-layout left
                                                     0
                                                     0
                                                     left-width
                                                     requested-height)
                   (layout/set-dimensions-and-layout middle
                                                     left-width
                                                     0
                                                     (:width middle-size)
                                                     requested-height)
                   (layout/set-dimensions-and-layout right
                                                     (+ left-width
                                                        (:width middle-size))
                                                     0
                                                     (max 0
                                                          (- requested-width
                                                             left-width
                                                             (:width middle-size)))
                                                     requested-height)]))))

    (preferred-size [this available-width available-height]
                    (let [middle-size (layoutable/preferred-size middle
                                                                 available-width
                                                                 available-height)
                          left-size (layoutable/preferred-size left
                                                               (max 0
                                                                    (min (- available-width
                                                                            (:width middle-size))
                                                                         left-width))
                                                               available-height)
                          right-size (layoutable/preferred-size right
                                                                (max 0
                                                                     (- available-width
                                                                        left-width
                                                                        (:width middle-size)))
                                                                available-height)]
                      {:width (+ (:width left-size)
                                 (:width middle-size)
                                 (:width right-size))

                       :height (max (:height left-size)
                                    (:height middle-size)
                                    (:height right-size))})))



#_(defn horizontal-split-view [view-context {:keys [left right left-width]}]
    (->HorizontalSplit left-width
                       [left

                        (assoc (drawable/->Rectangle 15 0 [0 0 255 255])
                          :on-drag (fn [state x y]
                                     (update-in state [:left-width] + x)))

                        right]))

#_(defn horizontal-split [view-context]
    {:local-state {:left-width 100}})

#_(defn scroll-bar [value maximum size width height on-drag]
    (let [margin 5]
      (layouts/->Superimpose [(drawable/->Rectangle 30 50 [255 255 255 255])
                              (layouts/->Preferred (layouts/->Margin (* height (/ value maximum))
                                                                     margin
                                                                     0
                                                                     margin
                                                                     [(assoc (drawable/->Rectangle (- width (* 2 margin))
                                                                                                   (* height
                                                                                                      (/ size
                                                                                                         maximum))
                                                                                                   [128 128 128 255])
                                                                        :on-drag (fn [state x y]
                                                                                   (println "dragging scroll" x y)
                                                                                   (let [change (* y
                                                                                                   (/ maximum
                                                                                                      height))
                                                                                         new-value (float (min (- maximum
                                                                                                                  size)
                                                                                                               (max 0
                                                                                                                    (+ value
                                                                                                                       change))))]
                                                                                     (on-drag state new-value))))]))])))

#_(defn scroll-panel-view [view-context {:keys [content]}]
    (let [scroll-bar-width 30]
      (layouts/->SizeDependent (fn [available-width available-height]
                                 {:width available-width
                                  :height (:height (layoutable/preferred-size content
                                                                              (- available-width scroll-bar-width)
                                                                              available-height))})

                               (fn [state requested-width requested-height]
                                 (let [maximum (:height (layoutable/preferred-size content
                                                                                   (- requested-width 30)
                                                                                   requested-height))
                                       scroll-position (max 0
                                                            (min (:scroll-position state)
                                                                 (- maximum requested-height)))]

                                   (gui/apply-to-current-view-state assoc :scroll-position scroll-position)

                                   (layouts/->FloatRight [(layouts/->Margin (- (:scroll-position state)) 0 0 0
                                                                            [content])
                                                          (scroll-bar scroll-position
                                                                      maximum
                                                                      requested-height
                                                                      scroll-bar-width
                                                                      requested-height
                                                                      (fn [state new-value]
                                                                        (assoc state :scroll-position new-value)))]))))))

#_(defn scroll-panel [view-context]
    {:local-state {:scroll-position 0}
     :view #'scroll-panel-view})

(defn text [value]
  (layouts/->Margin 2 2 0 0
                    [(drawable/->Text (str value)
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      [255 255 255 255])]))

#_(gui/def-control root
    ([view-context control-channel]
       {})

    ([view-context state]
       (horizontal-split :split {:left (scroll-panel :left-panel {:content (layouts/->Flow (for [i (range 10000 10030)]
                                                                                             (text i)))} )
                                 :right (scroll-panel :right-panel {:content (layouts/->Flow (for [i (range 20000 20030)]
                                                                                               (text i)))})})))

(defn barless-scroll-panel-view [view-context state]
  (layouts/->Margin (- (:scroll-position state)) 0 0 0
                    [(-> (:content state)
                         (gui/add-mouse-event-handler-with-context view-context
                                                                   (fn [state event]
                                                                     (if (= (:type event)
                                                                            :mouse-wheel-moved)
                                                                       #_(do (println "moved" (:time event))
                                                                             state)
                                                                       (update-in state [:scroll-position] (fn [position]
                                                                                                             (max 0 (+ position
                                                                                                                       (:y-distance event)))))
                                                                       state))))]))

(defn barless-scroll-panel [view-context]
  {:local-state {:scroll-position 0}
   :view #'barless-scroll-panel-view})

(defn barless-root-view [view-context state]
  (gui/call-view barless-scroll-panel
                 :scroll-panel
                 {:content (l/vertically (for [i (range 20)]
                                           (controls/text i)))
                  #_(l/vertically (for [i (range 5)]
                                    (gui/call-and-bind view-context state i :text controls/text-editor i)))}))

(defn barless-root [view-context]
  {:view #'barless-root-view})

#_(flow-gl.debug/set-active-channels :all)

(defn start []
  #_(.start (Thread. (fn []
                     (trace/trace-ns 'flow-gl.gui.gui)
                     #_(trace/trace-var* 'flow-gl.gui.gui/resolve-view-calls)
                     (trace/with-trace
                       (gui/start-control barless-root)))))

  (.start (Thread. (fn []
                     (gui/start-control barless-root))))

  #_(profiler/with-profiler (gui/start-control barless-root)))


#_(schema.core/with-fn-validation (trace/with-trace
                                    (println "foo" (schema.core/fn-validation?))
                                    (Thread/sleep 5000)
                                    (println "foo 2" (schema.core/fn-validation?))))



#_(run-tests)

;; flow-gl.debug/debug-timed

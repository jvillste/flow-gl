(ns examples.editor
  (:require (flow-gl.gui [input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            (flow-gl.opengl.jogl [quad :as quad]
                                 [render-target :as render-target]
                                 [opengl :as opengl])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font])
            [clojure.string :as string])
  (:import [javax.media.opengl GL2]
           [java.text AttributedString]
           [java.awt.font TextAttribute LineBreakMeasurer]
           [java.awt.image BufferedImage])
  (:use flow-gl.utils
        clojure.test))

(def font (font/create "LiberationSans-Regular.ttf" 15))

(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    font
                    color)))

(defn match-keyboard-event [event pattern]
  (let [values (select-keys event (keys pattern))]
    (= values pattern)))

(defn match-key-pressed [event modifiers key]
  (match-keyboard-event event
                        {:key key
                         :type :key-pressed
                         :control (contains? modifiers :control)
                         :shift (contains? modifiers :shift)
                         :alt (contains? modifiers :alt)}))

(defn cursor-x [editor-text cursor-row cursor-column]
  (-> (nth editor-text cursor-row)
      (.substring 0 cursor-column)
      (text)
      (layoutable/preferred-size java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE)
      (:width)))

(defn create-text-editor-keyboard-event-handler [view-context]
  (fn [state event]
    (println event)
    
    (cond
      (events/key-pressed? event :back-space)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (-> local-state
                                                         (update-in [:text (:cursor-row local-state)] (fn [line] (str (.substring line 0 (max 0 (dec (:cursor-column local-state))))
                                                                                                                      (.substring line (:cursor-column local-state)))))
                                                         (update-in [:cursor-column] (fn [cursor-column] (max 0 (dec cursor-column)))))))

      (match-key-pressed event #{:control} :n)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (let [cursor-position (cursor-x (:text local-state)
                                                                                     (:cursor-row local-state)
                                                                                     (:cursor-column local-state))
                                                           new-cursor-row (min (inc (:cursor-row local-state))
                                                                               (count (:text local-state)))]
                                                       (assoc local-state
                                                              :cursor-row new-cursor-row
                                                              :cursor-column (font/character-index-at-position font
                                                                                                               (nth (:text local-state)
                                                                                                                    new-cursor-row)
                                                                                                               cursor-position)))))

      (match-key-pressed event #{:control} :p)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (let [cursor-position (cursor-x (:text local-state)
                                                                                     (:cursor-row local-state)
                                                                                     (:cursor-column local-state))
                                                           new-cursor-row (max 0 (dec (:cursor-row local-state)))]
                                                       (assoc local-state
                                                              :cursor-row new-cursor-row
                                                              :cursor-column (font/character-index-at-position font
                                                                                                               (nth (:text local-state)
                                                                                                                    new-cursor-row)
                                                                                                               cursor-position)))))



      (match-key-pressed event #{:control} :f)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (update-in local-state [:cursor-column] (fn [cursor-column]
                                                                                               (min (inc cursor-column)
                                                                                                    (count (get-in local-state [:text (:cursor-row local-state)])))))))

      (match-key-pressed event #{:control} :b)
      (gui/apply-to-local-state state view-context update-in [:cursor-column] (fn [cursor-column] (max 0 (dec cursor-column))))
      
      (and (:character event)
           (= (:type event)
              :key-pressed))
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (-> local-state
                                                         (update-in [:text] (fn [text]
                                                                              (println "text is" text (:cursor-row local-state))
                                                                              (update-in text [(:cursor-row local-state)]
                                                                                         (fn [line]
                                                                                           (println "line is" line (:cursor-column local-state) (:cursor-row local-state))
                                                                                           (str (.substring line 0 (:cursor-column local-state))
                                                                                                (:character event)
                                                                                                (.substring line (+ (:cursor-column local-state))))))))
                                                         (update-in [:cursor-column] inc))))

      :default
      state)))


(defn line-offsets [text font width]
  (let [attributed-string (AttributedString. text)]
    (.addAttribute attributed-string TextAttribute/FONT (:font font))
    (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))
          attributed-character-iterator (.getIterator attributed-string)
          line-break-measurer (LineBreakMeasurer. attributed-character-iterator
                                                  (.getFontRenderContext graphics))]
      (loop [position 0
             positions []]
        (.setPosition line-break-measurer position)
        (let [position (.nextOffset line-break-measurer width)]
          (if (> (.length text)
                 position)
            (recur position
                   (conj positions position))
            positions))))))

(defn break-lines-by-offsets [text offsets]
  (loop [lines []
         start 0
         offsets offsets]
    (if-let [offset (first offsets)]
      (recur (conj lines (.trim (subs text start offset)))
             offset
             (rest offsets))
      (conj lines (.trim (subs text start))))))

(defn break-lines [text font width]
  (break-lines-by-offsets text (line-offsets text font width)))

(defrecord TextLayout [contents]
  layout/Layout
  (layout [this application-state requested-width requested-height]
    (let [child-layoutable (l/vertically (for [line (break-lines contents  font requested-width)]
                                           (text line)))
          [application-state child-layout] (layout/set-dimensions-and-layout child-layoutable
                                                                             application-state
                                                                             0
                                                                             0
                                                                             requested-width
                                                                             requested-height)]
      [application-state
       (assoc this :children [child-layout])]))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    (layoutable/preferred-size (l/vertically (for [line (break-lines contents font available-width)]
                                               (text line)))
                               available-width available-height)))



(defn text-editor-view [view-context state]
  (l/vertically (for [line (:text state)]
                  (l/margin 0 0 10 0 (->TextLayout line))))
  
  #_(let [{character-width :width character-height :height} (layoutable/preferred-size (text "a") 100 100)]
      (l/superimpose (l/vertically (for [line (:text state)]
                                     (text line)))
                     (l/absolute (assoc (drawable/->Rectangle 2 character-height [0 255 0 255])
                                        :x (cursor-x (:text state) (:cursor-row state) (:cursor-column state))
                                        :y (* character-height (:cursor-row state)))))))

(defn text-editor [view-context]
  {:local-state {:text  ["1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16" "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16"]
                 :cursor-row 0
                 :cursor-column 0}
   :handle-keyboard-event (create-text-editor-keyboard-event-handler view-context)
   :can-gain-focus true
   :view text-editor-view})


(defn start []
  #_(.start (Thread. (fn []
                       (trace/untrace-ns 'flow-gl.gui.gui)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus-if-can-gain-focus)
                       (trace/trace-var* 'flow-gl.gui.gui/set-focus)
                       #_(trace/trace-var* 'flow-gl.gui.gui/resolve-size-dependent-view-calls)
                       (trace/with-trace
                         (gui/start-control editor)))))
  
  (.start (Thread. (fn []
                     (gui/start-control text-editor))))

  #_(profiler/with-profiler (gui/start-control editor)))



(comment 

  (defn handle-editing-event [state editor event]
    (cond
      (input/key-pressed? event input/escape)
      (-> state
          (dataflow/define-to (concat editor [:cursor-position]) 0)
          (dataflow/define-to (concat editor [:editing]) false)
          (dataflow/define-to (concat editor [:edited-value]) (get state (concat editor [:value])))
          (assoc :event-handled true))

      (input/key-pressed? event input/left)
      (dataflow/apply-to-value state (concat editor [:cursor-position]) dec)

      (input/key-pressed? event input/right)
      (dataflow/apply-to-value state (concat editor [:cursor-position]) inc)

      (not (nil? (:character event)))
      (-> state
          (dataflow/apply-to-value (concat editor [:edited-value]) (fn [edited-value]
                                                                     (-> (StringBuffer. edited-value)
                                                                         (.insert (get state (concat editor [:cursor-position])) (:character event))
                                                                         (.toString))))

          (dataflow/apply-to-value  (concat editor [:cursor-position]) inc)
          (assoc :event-handled true))



      :default state))

  (defn handle-editor-event [state editor event]
    (cond

      (input/key-pressed? event input/enter)
      (if (dataflow/property-from state editor :editing)
        ((dataflow/property-from state editor :change-listener)
         (dataflow/define-to state
           (concat editor [:value]) (dataflow/property-from state editor :edited-value)
           (concat editor [:cursor-position]) 0
           (concat editor [:editing]) false)
         (dataflow/property-from state editor :edited-value))
        (dataflow/define-to state (concat editor [:editing]) true))

      :default (if (get state (concat editor [:editing]))
                 (handle-editing-event state editor event)
                 state)))


  (defn cursor [editor font]
    (let [text (dataflow/property editor :edited-value)
          cursor-position (dataflow/property editor :cursor-position)
          width 1
          height (font/height font)]
      (layout/->Translation (font/width font (subs text 0 cursor-position))
                            0
                            (drawable/->Rectangle width
                                                  height
                                                  #_(let [duration (* 1.5 1e9)
                                                          value (-> (dataflow/get-global-value :time)
                                                                    (mod duration)
                                                                    (/ duration)
                                                                    (* 2 Math/PI)
                                                                    (Math/sin)
                                                                    (/ 2)
                                                                    (+ 0.5))]
                                                      [1 value value 1])
                                                  [1 0 0 1]
                                                  ))))


  (defn editor [value change-listener]
    (let [font (font/create "LiberationSans-Regular.ttf" 15)
          editor-path (dataflow/absolute-path [])]
      (dataflow/initialize
       :value value
       :edited-value value
       :editing false
       :cursor-position 0
       :change-listener (fn [] change-listener))

      (view/initialize-view-part :cursor #(cursor editor-path
                                                  font))

      (let [text (if (dataflow/get-value :editing)
                   (dataflow/get-value :edited-value)
                   (dataflow/get-value :value))]
        (layout/->Box 2
                      (drawable/->Rectangle 0
                                            0
                                            [1 1 1 1])

                      (layout/->Stack (concat (if (dataflow/get-value :editing)
                                                [(view/call-view-part :cursor)]
                                                [])
                                              [(drawable/->Text text
                                                                font
                                                                [0 0 0 1])]))))))



  (defn view []
    (editor "Foo" (fn [state new-value]
                    (println new-value)
                    state)))

  (defn initialize [state state-atom]
    state)

  (defn start []
    (application/start view
                       :handle-event handle-editor-event
                       :initialize initialize
                       :framerate 160))

  (defn refresh []
    (when @application/state-atom-atom
      (swap! @application/state-atom-atom
             view/set-view
             view)))

  (refresh)

  (defn start-async []
    (.start (Thread. start)))

  (comment
    (start-async)
    (start)
    )
  )

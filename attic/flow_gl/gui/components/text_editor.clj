(ns flow-gl.gui.components.text-editor
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
  (:import [com.jogamp.opengl GL2]
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

(defn cursor-x [line-text cursor-column]
  (-> line-text
      (.substring 0 cursor-column)
      (text)
      (layoutable/preferred-size java.lang.Integer/MAX_VALUE java.lang.Integer/MAX_VALUE)
      (:width)))




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

(defn row-and-column [offset line-breaks]
  (loop [line-breaks line-breaks
         row 0
         previous-line-break 0]
    (if-let [line-break (first line-breaks)]
      (if (< offset line-break)
        [row (- offset previous-line-break)]
        (recur (rest line-breaks)
               (inc row)
               line-break))
      [row (- offset previous-line-break)])))

(defn create-text-editor-keyboard-event-handler [view-context]
  (fn [state event]
    (cond
      (events/key-pressed? event :back-space)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (-> local-state
                                                         (update-in [:paragraphs (:cursor-paragraph local-state)] (fn [paragraph] (str (.substring paragraph 0 (max 0 (dec (:cursor-offset local-state))))
                                                                                                                                       (.substring paragraph (:cursor-offset local-state)))))
                                                         (update-in [:cursor-offset] (fn [cursor-offset] (max 0 (dec cursor-offset)))))))

      (match-key-pressed event #{:control} :n)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (let [cursor-position (cursor-x (:paragraphs local-state)
                                                                                     (:cursor-row local-state)
                                                                                     (:cursor-column local-state))
                                                           new-cursor-row (min (inc (:cursor-row local-state))
                                                                               (count (:paragraphs local-state)))]
                                                       (assoc local-state
                                                              :cursor-row new-cursor-row
                                                              :cursor-column (font/character-index-at-position font
                                                                                                               (nth (:paragraphs local-state)
                                                                                                                    new-cursor-row)
                                                                                                               cursor-position)))))

      (match-key-pressed event #{:control} :p)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (let [cursor-position (cursor-x (:paragraphs local-state)
                                                                                     (:cursor-row local-state)
                                                                                     (:cursor-column local-state))
                                                           new-cursor-row (max 0 (dec (:cursor-row local-state)))]
                                                       (assoc local-state
                                                              :cursor-row new-cursor-row
                                                              :cursor-column (font/character-index-at-position font
                                                                                                               (nth (:paragraphs local-state)
                                                                                                                    new-cursor-row)
                                                                                                               cursor-position)))))



      (match-key-pressed event #{:control} :f)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (if (< (:cursor-offset local-state)
                                                            (.length (get (:paragraphs local-state) (:cursor-paragraph local-state))))
                                                       (update-in local-state [:cursor-offset] inc)
                                                       (if (< (:cursor-paragraph local-state)
                                                              (dec (count (:paragraphs local-state))))
                                                         (assoc local-state
                                                                :cursor-paragraph (inc (:cursor-paragraph local-state))
                                                                :cursor-offset 0)
                                                         local-state))))

      (match-key-pressed event #{:control} :b)
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (if (> (:cursor-offset local-state)
                                                            0)
                                                       (update-in local-state [:cursor-offset] dec)
                                                       (if (> (:cursor-paragraph local-state)
                                                              0)
                                                         (assoc local-state
                                                                :cursor-paragraph (dec (:cursor-paragraph local-state))
                                                                :cursor-offset (.length (get (:paragraphs local-state) (dec (:cursor-paragraph local-state)))))
                                                         local-state))))

      
      (and (:character event)
           (= (:type event)
              :key-pressed))
      (gui/apply-to-local-state state view-context (fn [local-state]
                                                     (-> local-state
                                                         (update-in [:paragraphs (:cursor-paragraph local-state)] (fn [text]
                                                                                                                    (str (.substring text 0 (:cursor-offset local-state))
                                                                                                                         (:character event)
                                                                                                                         (.substring text (+ (:cursor-offset local-state))))))
                                                         (update-in [:cursor-offset] inc))))

      :default
      state)))

(defn character-height []
  (:height (layoutable/preferred-size (text "a")
                                      java.lang.Integer/MAX_VALUE
                                      java.lang.Integer/MAX_VALUE)))

(defn paragraph-layoutable [lines font color]
  (l/vertically (for [line lines]
                  (drawable/->Text (str line)
                                   font
                                   color))))

(defn preferred-paragraph-size [contents font available-width available-height]
  (layoutable/preferred-size (paragraph-layoutable (break-lines contents font available-width)
                                                   font
                                                   [255 255 255 255])
                             available-width available-height))

(defrecord ParagraphWithCursor [contents font color cursor-offset cursor-layoutable-function]
  layout/Layout
  (layout [this application-state requested-width requested-height]
    (let [character-height (character-height)
          line-offsets (line-offsets contents font requested-width)
          lines (break-lines-by-offsets contents line-offsets)
          child-layoutable (paragraph-layoutable lines font color)
          child-layoutable (let [[cursor-row cursor-column] (row-and-column cursor-offset line-offsets)]
                             (l/superimpose
                              child-layoutable
                              (l/absolute (assoc (cursor-layoutable-function character-height)
                                                 :x (cursor-x (get lines cursor-row) cursor-column)
                                                 :y (* character-height cursor-row)))))
          
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
    (preferred-paragraph-size contents font available-width available-height)))

(defrecord Paragraph [contents font color]
  layout/Layout
  (layout [this application-state requested-width requested-height]
    (let [[application-state child-layout] (layout/set-dimensions-and-layout (-> contents
                                                                                 (break-lines font requested-width)
                                                                                 (paragraph-layoutable font color))
                                                                             application-state
                                                                             0
                                                                             0
                                                                             requested-width
                                                                             requested-height)]
      [application-state
       (assoc this :children [child-layout])]))

  layoutable/Layoutable
  (preferred-size [this available-width available-height]
    (preferred-paragraph-size contents font available-width available-height)))


(defn text-editor-view [view-context state]
  (l/vertically (for [[paragraph index] (partition 2 (interleave (:paragraphs state) (iterate inc 0)))]
                  (l/margin 0 0 10 0 (if (= (:cursor-paragraph state) index)
                                       (->ParagraphWithCursor paragraph
                                                              font
                                                              [255 255 255 255]
                                                              (:cursor-offset state)
                                                              (fn [character-height]
                                                                (drawable/->Rectangle 2
                                                                                      character-height
                                                                                      [0 255 0 255])))
                                       (->Paragraph paragraph
                                                    font
                                                    [255 255 255 255]))))))

(defn text-editor [view-context]
  {:local-state {:paragraphs  ["1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16" "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16"]
                 :cursor-paragraph 0
                 :cursor-offset 4}
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

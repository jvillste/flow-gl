(ns fungl.component.text-area
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [flow-gl.graphics.font :as font]
            [flow-gl.graphics.text :as text]
            [flow-gl.gui.animation :as animation]
            [flow-gl.gui.keyboard :as keyboard]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.cache :as cache]
            [fungl.handler :as handler]
            [fungl.layouts :as layouts]
            [fungl.value-registry :as value-registry]
            [fungl.layout :as layout]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.string :as string]
            [fungl.clipboard :as clipboard])
  (:import (java.awt.font TextHitInfo)))

(defn default-font []
  (font/create (.getPath (io/resource "LiberationSans-Regular.ttf")) 18))

(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (text value color (default-font)))

  ([value color font]
   (visuals/text-area (str value)
                      color
                      font)))

(defn get-rows-node-size [node]
  (text/rows-size (:rows node)))

(defn rows-node [rows]
  {:rows rows
   :get-size get-rows-node-size

   :image-function text/create-buffered-image-for-rows
   :draw-function text/draw-rows
   :image-function-parameter-keys [:rows]})

(defn trailing-x [row index]
  (first (.getCaretInfo (:layout row)
                        #_(TextHitInfo/leading index)
                        (TextHitInfo/trailing index))))

(defn leading-x [row index]
  (first (.getCaretInfo (:layout row)
                        (TextHitInfo/leading index))))

(comment (trailing-x (first (rows [255 255 255 255]
                                  font
                                  "ab" #_"one two three five six seven eight nine ten"
                                  100))
                     2))

(defn character-position [rows index]
  (loop [y 0
         rows rows
         row-number 0]
    (if-let [row (first rows)]
      (if (and (>= index (:from row))
               (< index (:to row)))
        {:x (leading-x row (- index (:from row)))
         :y y
         :row-number row-number
         :height (text/row-height row)}
        (if (empty? (rest rows))
          {:x (trailing-x row
                          (dec (text/row-length row)))
           :y y
           :row-number row-number
           :height (text/row-height row)}
          (recur (+ y (text/row-height row))
                 (rest rows)
                 (inc row-number))))
      nil)))

(comment (character-position (rows [255 255 255 255]
                                   font
                                   "ab" #_"one two three five six seven eight nine ten"
                                   100)
                             3))

(defn index-at-coordinates [rows x y]
  (loop [row-y 0
         rows rows]
    (if-let [row (first rows)]
      (if (and (>= y row-y)
               (< y (+ row-y (text/row-height row))))
        (+ (:from row)
           (.getInsertionIndex (.hitTestChar (:layout row)
                                             x
                                             (- y row-y))))
        (recur (+ row-y (text/row-height row))
               (rest rows)))
      nil)))

(defn index-at-previous-row [rows x index]
  (if-let [position (character-position rows index)]
    (if (< 0 (:row-number position))
      (index-at-coordinates rows
                            x
                            (- (:y position)
                               (text/row-height (nth rows (:row-number position)))))
      index)
    index))

(defn index-at-the-end-of-the-row [rows index]
  (if-let [position (character-position rows index)]
    (:to (nth rows (:row-number position)))
    0))

(defn index-at-the-beginning-of-the-row [rows index]
  (if-let [position (character-position rows index)]
    (:from (nth rows (:row-number position)))
    0))


(defn index-at-next-row [rows x index]
  (if-let [position (character-position rows index)]
    (if (> (dec (count rows))
           (:row-number position))
      (index-at-coordinates rows
                            x
                            (+ (:y position)
                               (text/row-height (nth rows (:row-number position)))))
      index)
    index))

(defn insert [target index source]
  (-> (StringBuilder. target)
      (.insert index
               source)
      (.toString)))

(defn delete [target from to]
  (-> (StringBuilder. target)
      (.delete from
               to)
      (.toString)))

(deftest delete-string-test
  (is (= "ade"
         (delete "abcde" 1 3)))

  (is (= "acde"
         (delete "abcde" 1 2)))

  (is (= "abcd"
         (delete "abcde" 4 5))))


;; Commands

(defn previous-row [state rows]
  (assoc state :index (index-at-previous-row rows
                                             (:x-on-first-line-change state)
                                             (:index state))))
(defn next-row [state rows]
  (assoc state :index (index-at-next-row rows
                                         (:x-on-first-line-change state)
                                         (:index state))))

(defn move-to-the-end-of-the-row [state rows]
  (assoc state :index (index-at-the-end-of-the-row rows (:index state))))

(defn move-to-the-beginning-of-the-row [state rows]
  (assoc state :index (index-at-the-beginning-of-the-row rows (:index state))))

(defn backward [state rows]
  (update state :index (fn [index]
                         (max 0 (dec index)))))

(defn forward [state rows]
  (update state :index (fn [index]
                         (min (:to (last rows))
                              (inc index)))))

(defn delete-backward [state rows]
  (if (< 0 (:index state))
    (-> state
        (update :text
                delete
                (dec (:index state))
                (:index state))
        (update :index dec))
    state))

(defn delete-forward [state rows]
  (if (< (:index state)
         (count (:text state)))
    (-> state
        (update :text
                delete
                (:index state)
                (inc (:index state))))
    state))

(defn paste [state _rows]
  (-> state
      (update :text
              insert
              (:index state)
              (clipboard/slurp-clipboard))))

(defn insert-string [state rows character]
  (-> state
      (update :text
              insert
              (:index state)
              (str character))
      (update :index inc)))

;; Commands end


(defn handle-command [state command & parameters]

  (let [state (if (#{previous-row next-row} (if (var? command)
                                              @command
                                              command))
                (assoc state :x-on-first-line-change (or (:x-on-first-line-change state)
                                                         (:x (character-position (:rows state) (:index state)))))
                (dissoc state :x-on-first-line-change))]
    (apply command
           state
           (:rows state)
           parameters)))

(defn keyboard-event-to-command [state event]
  (when (= :key-pressed
           (:type event))
    (cond (or (= :up (:key event))
              (and (= :p (:key event))
                   (:control? event)))
          [previous-row]

          (and (or (= :down (:key event))
                   (and (= :n (:key event))
                        (:control? event)))
               (not (= (:index state)
                       (index-at-next-row (:rows state)
                                          (:x-on-first-line-change state)
                                          (:index state)))))
          [next-row]

          (or (= :left (:key event))
              (and (= :b (:key event))
                   (:control? event)))
          [backward]

          (or (= :right (:key event))
              (and (= :f (:key event))
                   (:control? event)))
          [forward]

          (= :back-space (:key event))
          [delete-backward]

          (and (= :d (:key event))
               (:control? event))
          [delete-forward]

          (and (= :e (:key event))
               (:control? event)
               (not (:meta? event)))
          [move-to-the-end-of-the-row]

          (and (= :a (:key event))
               (:control? event))
          [move-to-the-beginning-of-the-row]

          (and (= :v (:key event))
               (:meta? event))
          [paste]

          :else
          (when-let [character (:character event)]
            (when (and (not (:control? event))
                       (not (:alt? event))
                       (not (:meta? event))
                       (not (empty? (string/replace (str character)
                                                    #"\p{C}" ;; from https://stackoverflow.com/a/62915361
                                                    ""))))
              [insert-string character])))))

(defn initialize-state [] {:index 0})

(def atom-specification
  {:create initialize-state})

(defn default-style []
  {:font (default-font)
   :color [100 100 100 255]})

#_(handler/def-handler-creator text-area-keyboard-event-handler [state-atom on-change rows] [event]
    (prn event)
    (when-let [command-and-parameters (keyboard-event-to-command event)]
      (swap! state-atom
             (fn [state]
               (on-change state
                          (apply handle-command
                                 state
                                 rows
                                 command-and-parameters))))))

(defn text-area-keyboard-event-handler [state-atom text on-change _node event]
  (if-let [command-and-parameters (keyboard-event-to-command @state-atom event)]
    (do (when on-change
          (swap! state-atom
                 (fn [state]
                   (on-change state
                              (apply handle-command
                                     state
                                     command-and-parameters)))))
        nil)
    event))

(defn text-area-mouse-event-handler [state-atom rows node event]
  (if (= (:type event)
         :mouse-clicked)
    (do (keyboard/set-focused-node! node)
        (when-let [index (index-at-coordinates rows
                                               (:local-x event)
                                               (:local-y event))]
          (swap! state-atom (fn [state]
                              (assoc state
                                     :index index))))))
  event)

(defn adapt-to-space [text index style handle-rows node]
  (let [style (conj (default-style)
                    style)
        rows (if (empty? text)
               []
               (cache/call! text/rows-for-text
                            (:color style)
                            (:font style)
                            text
                            (:available-width node)))]

    (when handle-rows
      (handle-rows rows))

    (merge (dissoc node :adapt-to-space)
           (layouts/with-minimum-size (font/width (:font style) "W") (font/height (:font style))
             (layouts/superimpose (when index
                                    (let [rectangle (visuals/rectangle (:color style) 0 0)]
                                      (if (empty? rows)
                                        (assoc rectangle
                                               :width 4
                                               :x 0
                                               :y 0
                                               :height (font/height (:font style)))
                                        (let [caret-position (character-position rows index)]
                                          (assoc rectangle
                                                 :width 4
                                                 :x (:x caret-position)
                                                 :y (:y caret-position)
                                                 :height (:height caret-position))))))
                                  (rows-node rows))))))

(defn create-scene-graph [text index style handle-rows]
  (assert text)

  {:adapt-to-space [adapt-to-space text index style handle-rows]})

(defn text-area-for-state-atom [state-atom {:keys [text on-change style]}]
  (swap! state-atom assoc :text text)
  (let [state @state-atom]
    (-> (create-scene-graph text
                            (when (keyboard/component-is-focused?)
                              (:index state))
                            style
                            (fn [rows]
                              (swap! state-atom assoc :rows rows)))
        (assoc :mouse-event-handler [text-area-mouse-event-handler state-atom (:rows @state-atom)])
        (cond-> on-change
          (assoc :keyboard-event-handler [text-area-keyboard-event-handler state-atom text on-change]
                 :can-gain-focus? true)))))

(def default-options {:style {}
                      :text ""
                      :on-change (fn [_old-state new-state]
                                   new-state)})

(defn text-area-3 [_options]
  (let [state-atom (dependable-atom/atom "text-area-state" (initialize-state))]
    (fn [options]
      (when (not (= (:text options)
                    (:text @state-atom)))
        (swap! state-atom
               (fn [state]
                 (assoc state
                        :text (:text options)
                        :index (min (count (:text options))
                                    (:index state))))))

      (text-area-for-state-atom state-atom
                                (assoc (merge default-options
                                              options)
                                       :on-change (cond (:on-text-change options)
                                                        (fn [old-state new-state]
                                                          (when (not= (:text new-state) (:text old-state))
                                                            ((:on-text-change options) (:text new-state)))
                                                          new-state)

                                                        (:on-change options)
                                                        (:on-change options)

                                                        :else
                                                        (:on-change default-options)))))))

(defn demo-view []
  (let [state-atom (dependable-atom/atom {:text-1 (apply str
                                                         (repeat 10 "text 1 "))
                                          :text-2 "text 2"
                                          :text-3 "text 3"})]
    (fn []
      @animation/state-atom
      (animation/swap-state! animation/set-wake-up 1000)

      (layouts/superimpose (visuals/rectangle-2 :color [0 0 0 255])
                           (layouts/with-margins 10 10 10 10
                             [text-area-3 {:style {:color [255 255 255 255]}
                                           :text (:text-3 @state-atom)
                                           :on-text-change (fn [new-text]
                                                             (swap! state-atom assoc :text-3 new-text))}])))))

(defn start []
  (application/start-application #'demo-view))

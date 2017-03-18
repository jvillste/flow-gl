(ns fungl.component.text-area
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [application :as application]
                   [atom-registry :as atom-registry]
                   [layouts :as layouts]
                   [cache :as cache]
                   [handler :as handler])
            (flow-gl.graphics [font :as font]
                              [text :as text])
            (flow-gl.gui [animation :as animation]
                         [keyboard :as keyboard]
                         
                         
                         [visuals :as visuals]))
  (:import [java.awt.font TextHitInfo])
  (:use clojure.test))

(def font (font/create "LiberationSans-Regular.ttf" 18))

(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (visuals/text-area color
                      (font/create "LiberationSans-Regular.ttf" 15)
                      (str value))))

#_(defn rows [color font string width]
    (text/rows-for-text color
                        font
                        string
                        width))

(defn get-rows-node-size [node]
  (text/rows-size (:rows node)))

(defn rows-node [rows]
  {:rows rows
   :get-size get-rows-node-size
   
   :image-function text/create-buffered-image-for-rows
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
  (let [position (character-position rows index)]
    (if (< 0 (:row-number position))
      (index-at-coordinates rows
                            x
                            (- (:y position)
                               (text/row-height (nth rows (:row-number position))))) 
      index)))


(defn index-at-next-row [rows x index]
  (let [position (character-position rows index)]
    (if (> (dec (count rows))
           (:row-number position))
      (index-at-coordinates rows
                            x
                            (+ (:y position)
                               (text/row-height (nth rows (:row-number position))))) 
      index)))

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

(defn gain-focus [state rows]
  (assoc state :has-focus true))

(defn loose-focus [state rows]
  (assoc state :has-focus false))

(defn insert-string [state rows character]
  (-> state
      (update :text
              insert
              (:index state)
              (str character))
      (update :index inc)))

;; Commands end


(defn handle-command [state rows command & parameters]

  (let [state (if (#{previous-row next-row} (if (var? command)
                                              @command
                                              command))
                (assoc state :x-on-first-line-change (or (:x-on-first-line-change state)
                                                         (:x (character-position rows (:index state)))))
                (dissoc state :x-on-first-line-change))]
    (apply command
           state
           rows
           parameters)))

(defn keyboard-event-to-command [event]
  (if (= :key-pressed
         (:type event))
    (case (:key event)
      :up
      [previous-row]
      
      :down
      [next-row]

      :left
      [backward]

      :right
      [forward]

      :back-space
      [delete-backward]

      (if-let [character (:character event)]
        [insert-string character]
        nil))
    (case (:type event)
      :focus-gained [gain-focus]
      :focus-lost [loose-focus]
      nil)))

(defn initialize-state [] {:index 0})

(def atom-specification
  {:create initialize-state})

(def default-style {:font font
                    :color [100 100 100 255]})

(handler/def-handler-creator create-text-area-keyboard-event-handler [state-atom on-change rows] [event]
  (when-let [command-and-paramters (keyboard-event-to-command event)]
    (swap! state-atom
           (fn [state]
             (on-change state
                        (apply handle-command
                               state
                               rows
                               command-and-paramters))))))

(handler/def-handler-creator create-text-area-mouse-event-handler [state-atom rows] [node event]
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

(handler/def-handler-creator create-adapt-to-space [text index style handle-rows] [node]
  (let [style (conj default-style
                    style)
        rows (cache/call!  text/rows-for-text
                           (:color style)
                           (:font style)
                           text
                           (:available-width node))]

    (when handle-rows
      (handle-rows rows))

    (conj node
          (layouts/superimpose (when index
                                 (let [caret-position (character-position rows index)]
                                   (assoc (visuals/rectangle (:color style) 0 0)
                                          :width 1
                                          :x (:x caret-position)
                                          :y (:y caret-position)
                                          :height (:height caret-position))))
                               (rows-node rows)))))

(defn create-scene-graph [text index style handle-rows]
  (assert text)
  
  {:adapt-to-space (create-adapt-to-space text index style handle-rows)})


(defn text-area [id style text on-change & options]
  (let [state-atom (atom-registry/get! id atom-specification)]

    (swap! state-atom assoc :text text)
    (let [state @state-atom]
      (assoc (create-scene-graph (:text state)
                                 (when (:has-focus state)
                                   (:index state))
                                 style
                                 (fn [rows]
                                   (swap! state-atom assoc :rows rows)))
             :id id
             :mouse-event-handler (create-text-area-mouse-event-handler state-atom
                                                                        (:rows @state-atom))
             :keyboard-event-handler (create-text-area-keyboard-event-handler state-atom
                                                                              on-change
                                                                              (:rows @state-atom))))))



(defn create-demo-scene-graph [width height]

  (animation/swap-state! animation/set-wake-up 1000)

  (let [state-atom (atom-registry/get! :root {:create (fn []
                                                        {:text-1 (apply str
                                                                        (repeat 10 "foo bar "))
                                                         :text-2 "text 2"})})]
    (-> (layouts/with-margins 10 10 10 10
          (layouts/vertically
           (layouts/with-margins 0 0 10 0
             (text-area :area-1
                        {:color [255 255 255 255]}
                        (:text-1 @state-atom)
                        (fn [old-state new-state]
                          (swap! state-atom assoc :text-1 (:text new-state))
                          new-state)))
           
           (text-area :area-2
                      {:color [255 255 255 255]}
                      (:text-2 @state-atom)
                      (fn [old-state new-state]
                        (swap! state-atom assoc :text-2 (:text new-state))
                        new-state))
           
           #_(text (prn-str @state-atom))))
        (application/do-layout width height))))

(defn start []
  #_(let [layout (first (rows [255 255 255 255]
                              font
                              "Wicd"
                              140))
          text-hit-info (.getNextLeftHit layout
                                         3)]
      (seq (.getCaretInfo layout
                          text-hit-info)))

  #_(0.0 0.0 0.0 -16.589355 0.0 3.5200195)
  #_(10.0 0.0 10.0 -16.589355 10.0 3.5200195)
  #_(20.0 0.0 20.0 -16.589355 20.0 3.5200195)
  #_(-> 
     (first)
     #_(.getNextLeftHit 1)
     (.getCaretShapes 1)
     (first)
     #_(.getCharIndex))
  
  (application/start-window #'create-demo-scene-graph))

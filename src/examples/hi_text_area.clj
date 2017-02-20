(ns examples.hi-text-area
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [application :as application]
                   [atom-registry :as atom-registry]
                   [layouts :as layouts])
            (flow-gl.graphics [font :as font]
                              [text :as text])
            (flow-gl.gui [animation :as animation]
                         [keyboard :as keyboard]
                         
                         
                         [visuals :as visuals]))
  (:import [java.awt.font TextHitInfo]))

(def font (font/create "LiberationSans-Regular.ttf" 18))

(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (visuals/text-area color
                      (font/create "LiberationSans-Regular.ttf" 15)
                      (str value))))

(defn rows [color font string width]
  (text/rows-for-text color
                      font
                      string
                      width))

(defn rows-node [rows]
  {:rows rows
   :get-size (fn [node]
               (prn "rows-size" (text/rows-size (:rows node)))
               (text/rows-size (:rows node)))
   
   :image-function text/create-buffered-image-for-rows
   :image-function-parameter-keys [:rows]})

(defn caret-x [row index]
  (first (.getCaretInfo (:layout row)
                        #_(TextHitInfo/leading index)
                        (TextHitInfo/trailing index))))

(comment (caret-x (first (rows [255 255 255 255]
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
        {:x (caret-x row (- index (:from row)))
         :y y
         :row-number row-number
         :height (text/row-height row)}
        (recur (+ y (text/row-height row))
               (rest rows)
               (inc row-number)))
      nil)))

(comment (character-position (rows [255 255 255 255]
                                   font
                                   "ab" #_"one two three five six seven eight nine ten"
                                   100)
                             1))

(defn index-at-coordinates [rows x y]
  (loop [row-y 0
         rows rows]
    (if-let [row (first rows)]
      (if (and (>= y row-y)
               (< y (+ row-y (text/row-height row))))
        (+ (:from row)
           (.getCharIndex (.hitTestChar (:layout row)
                                        x
                                        (- y row-y))))
        (recur (+ row-y (text/row-height row))
               (rest rows)))
      nil)))

(defn index-at-previous-row [rows index]
  (let [position (character-position rows index)]
    (if (< 0 (:row-number position))
      (index-at-coordinates rows
                            (inc (:x position))
                            (- (:y position)
                               (text/row-height (nth rows (:row-number position))))) 
      index)))


(defn index-at-next-row [rows index]
  (let [position (character-position rows index)]
    (if (> (dec (count rows))
           (:row-number position))
      (index-at-coordinates rows
                            (inc (:x position))
                            (+ (:y position)
                               (text/row-height (nth rows (:row-number position))))) 
      index)))

(defn handle-action [state rows action & parameters]
  (case action
    :previous-row (assoc state :index (index-at-previous-row rows (:index state)))
    :next-row (assoc state :index (index-at-next-row rows (:index state)))
    :back (update state :index (fn [index]
                                 (max 0 (dec index))))
    :forward (update state :index (fn [index]
                                    (min (:to (last rows))
                                         (inc index))))
    state))

(defn keyboard-event-to-action [event]
  (if (= :key-pressed
         (:type event))
    (case (:key event)
      :up
      [:previous-row]
      
      :down
      [:next-row]

      :left
      [:back]

      :right
      [:forward]

      [:no-action])
    [:no-action]))

(defn create-scene-graph [width height]

  (animation/swap-state! animation/set-wake-up 1000)
  (-> (layouts/with-margins 10 10 10 10
        (let [state-atom (atom-registry/get! :root {:create (fn []
                                                              {:index 0
                                                               :text "one two three five six seven eight nine ten"})})
              state @state-atom
              rows (rows [255 255 255 255]
                         font
                         (:text state)
                         140)
              character-position (character-position rows (:index state))]
          
          (keyboard/set-focused-event-handler! (fn [event]
                                                 (swap! state-atom (fn [state]
                                                                     (apply handle-action state rows (keyboard-event-to-action event))))))
          
          (layouts/vertically 
           (layouts/superimpose (assoc (visuals/rectangle [255 255 255 255] 0 0)
                                       :width 1
                                       :x (:x character-position)
                                       :y (:y character-position)
                                       :height (:height character-position))
                                #_(assoc (visuals/rectangle [0 155 155 155] 0 0)
                                         :width (:width (text/layouts-size rows))
                                         :height (:height (text/layouts-size rows)))
                                (assoc (rows-node rows)
                                       :mouse-event-handler (fn [node event]
                                                              (if (= (:type event)
                                                                     :mouse-clicked)
                                                                (when-let [index (index-at-coordinates rows
                                                                                                       (:local-x event)
                                                                                                       (:local-y event))]
                                                                  (swap! state-atom assoc :index index)))
                                                              event))
                                #_(visuals/text-area [255 255 255 255]
                                                     font
                                                     text))
           (text (str "index:" (:index state)))))
        
        #_(layouts/box 10
                       (visuals/rectangle [255 255 255 255] 20 20)
                       {:children [(assoc (visuals/rectangle [0 255 255 255] 20 20)
                                          :width 20
                                          :height 20)
                                   (text "one two three five six seven eight nine ten")]}))
      (application/do-layout width height)))

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
  
  (application/start-window #'create-scene-graph))




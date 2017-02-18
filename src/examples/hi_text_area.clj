(ns examples.hi-text-area
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [application :as application]
                   [atom-registry :as atom-registry])
            (flow-gl.graphics [font :as font]
                              [text :as text])
            (flow-gl.gui [animation :as animation]
                         [layout :as layout]
                         [layouts :as layouts]
                         [visuals :as visuals])))

(def font (font/create "LiberationSans-Regular.ttf" 18))

(defn text
  ([value]
   (text value [0 0 0 255]))

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
               (text/rows-size (:rows node)))
   
   :image-function text/create-buffered-image-for-rows
   :image-function-parameter-keys [:rows]})

(defn caret-x [row index]
  (first (.getCaretInfo (:layout row)
                        (.getNextLeftHit (:layout row)
                                         (min (- (:to row)
                                                 (:from row))
                                              (inc index))))))

(defn character-bounds [rows index]
  (loop [y 0
         rows rows]
    (if-let [row (first rows)]
      (if (and (>= index (:from row))
               (< index (:to row)))
        {:x (caret-x row (- index (:from row)))
         :y y
         :height (text/row-height row)}
        (recur (+ y (text/row-height row))
               (rest rows)))
      nil)))

(comment (character-bounds (rows [255 255 255 255]
                                 font
                                 "one two three five six seven eight nine ten"
                                 100)
                           24))

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

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 1000)
  (application/do-layout 

   (layouts/with-margins 10 10 10 10
     (let [state-atom (atom-registry/get! :root {:create (fn [] {:index 0})})
           state @state-atom
           text "one two three five six seven eight nine ten"
           rows (rows [255 255 255 255]
                      font
                      text
                      140)
           character-bounds (character-bounds rows (:index state))]
       {:children [(assoc (visuals/rectangle [255 255 255 255] 0 0)
                          :width 1
                          :x (:x character-bounds)
                          :y (:y character-bounds)
                          :height (:height character-bounds))
                   #_(assoc (visuals/rectangle [0 155 155 155] 0 0)
                            :width (:width (text/layouts-size rows))
                            :height (:height (text/layouts-size rows)))
                   (rows-node rows)
                   #_(visuals/text-area [255 255 255 255]
                                        font
                                        text)]
        :mouse-event-handler (fn [node event]
                               (if (= (:type event)
                                      :mouse-clicked)
                                 (when-let [index (index-at-coordinates rows
                                                                        (:local-x event)
                                                                        (:local-y event))]
                                   (swap! state-atom assoc :index index)))
                               event)})
     
     #_(layouts/box 10
                    (visuals/rectangle [255 255 255 255] 20 20)
                    {:children [(assoc (visuals/rectangle [0 255 255 255] 20 20)
                                       :width 20
                                       :height 20)
                                (text "one two three five six seven eight nine ten")]}))
   width height))

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




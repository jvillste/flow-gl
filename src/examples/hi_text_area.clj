(ns examples.hi-text-area
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.graphics [font :as font]
                              [text :as text])
            (flow-gl.gui [layout :as layout]
                         [layouts :as layouts]
                         [visuals :as visuals])))

(def font (font/create "LiberationSans-Regular.ttf" 15))

(defn text
  ([value]
   (text value [0 0 0 255]))

  ([value color]
   (visuals/text-area color
                      (font/create "LiberationSans-Regular.ttf" 15)
                      (str value))))

(defn layouts [color font string width]
  (text/layouts-for-text color
                         font
                         string
                         width))

(defn layouts-node [layouts]
  {:layouts layouts
   :get-size (fn [node]
               (text/layouts-size (:layouts node)))
   
   :image-function text/create-buffered-image-for-layouts
   :image-function-parameter-keys [:layouts]})

(defn create-scene-graph [width height]
  (application/do-layout 

   (layouts/with-margins 10 10 10 10
     (let [layouts (layouts [255 255 255 255]
                            font
                            "one two three five six seven eight nine ten"
                            140)]
       {:children [(assoc (visuals/rectangle [0 155 155 155] 0 0)
                          :width (:width (text/layouts-size layouts))
                          :height (:height (text/layouts-size layouts)))
                   (layouts-node layouts)]})
     
     #_(layouts/box 10
                    (visuals/rectangle [255 255 255 255] 20 20)
                    {:children [(assoc (visuals/rectangle [0 255 255 255] 20 20)
                                       :width 20
                                       :height 20)
                                (text "one two three five six seven eight nine ten")]}))
   width height))

(defn start []
  (application/start-window #'create-scene-graph))




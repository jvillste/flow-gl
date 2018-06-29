(ns examples.hi-flow-layout
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
  (:import [java.awt.font TextHitInfo])
  (:use clojure.test))

(def font (font/create "LiberationSans-Regular.ttf" 18))


(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 1000)
  
  (-> (layouts/with-maximum-size 150 100 (layouts/vertically (apply layouts/flow (repeat 10 (visuals/text "haa ")))
                           (apply layouts/flow (repeat 10 (visuals/text "bar "))))) 
      (application/do-layout width height)))

(defn start []
  (application/start-window #'create-scene-graph))




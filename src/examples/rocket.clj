(ns examples.hi-animation
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layouts :as layouts]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [animation :as animation])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])))

(def font (font/create "LiberationSans-Regular.ttf" 15))

(def state (atom {:x 100
                  :y 100}))

(defn text-box [text]
  (assoc layouts/box
         :margin 5
         :children [(visuals/rectangle [155 155 255 255]
                                       5
                                       5)
                    (visuals/text [0 0 0 255]
                                  font
                                  text)]))


(defn animating-text-box [text mouse-handler animation]
  {:children [(conj (assoc (text-box text)
                           :y 10
                           :mouse-event-handler (fn [node event]
                                                  (when (= :mouse-clicked
                                                           (:type event))
                                                    (mouse-handler))
                                                  event))
                    animation)]
   
   :get-size (fn [node]
               {:width (:available-width node)
                :height 50})})

(def rocket (buffered-image/create-from-file "rocket.png"))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/start-if-not-running :rocket)

  {:children [(-> (visuals/image rocket)
                  (assoc :x (:x @state)
                         :y (:y @state)
                         :keyboard-event-handler (fn [event] (prn event))
                         :id :rocket))]
   :keyboard-event-handler (fn [event] (prn event))
   :id :root
   :x 0
   :y 0
   :width width
   :height height})


(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))


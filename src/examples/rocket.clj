(ns examples.hi-animation
  (:require [clojure.spec.test :as spec-test]

            [fungl.application :as application]
            [clojure.java.io :as io]
            (fungl [layouts :as layouts])
            (flow-gl.gui 
             [keyboard :as keyboard]
             [visuals :as visuals]
             [animation :as animation])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])))

(def font (font/create (.getPath (io/resource "LiberationSans-Regular.ttf")) 15))

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

(def rocket (buffered-image/create-from-file (.getPath (io/resource "rocket.png" #_"pumpkin.png"))))

(def angry (buffered-image/create-from-file "/Users/jukka/Pictures/angry.jpeg"))

#_(def angry2 (buffered-image/create-from-file "/Users/jukka/Desktop/Screen Shot 2017-06-19 at 21.47.18.png"))
(def siemenet (buffered-image/create-from-file "/Users/jukka/Google Drive/jukka/kuvalinnut/siemenet.png"))

(defonce state-atom (atom {:seeds [{:x 100 :y 100}
                                   {:x 200 :y 100}]}))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/start-if-not-running :rocket)
  (let [state state-atom]
    #_(animation/swap-state! animation/set-wake-up 1000)
    (application/do-layout {:children #_(for [seed (:seeds @state)])
                            [(-> (visuals/image siemenet #_angry #_rocket)
                                 (assoc :x (:x @state)
                                        :y (:y @state)
                                        :width 100
                                        :height 100
                                        #_:width #_(animation/linear-mapping (animation/ping-pong 5
                                                                                                  (animation/phase! :rocket
                                                                                                                    #_5000))
                                                                             0 width)  #_(* (animation/phase! :rocket) 200) 
                                        #_:height #_200
                                        :keyboard-event-handler (fn [event] (prn event))
                                        :id :rocket))]
                            :keyboard-event-handler (fn [event] (prn event))
                            :id :root
                            :x 0
                            :y 0
                            :width width
                            :height height}
                           width height))
  )

(defn handle-event [scene-graph event]
  (when (and (= :key-pressed (:type event))
             (= :keyboard (:source event)))
    (prn event)
    (case (:key event)
      :right (swap! state update :x (fn [x] (min 400 (+ x 10))))
      :left (swap! state update :x (fn [x] (max 0 (- x 10))))
      :up (swap! state update :y (fn [y] (max 0 (- y 10))))
      :down (swap! state update :y (fn [y] (min 500 (+ y 10))))
      ;;      :left (swap! state update :x (fn [x] (- x 10)))
      nil)))

(defn start []
  #_(spec-test/instrument)
  #_(spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :handle-event #'handle-event)))))


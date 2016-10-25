(ns examples.hi-animation
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layouts :as layouts]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [animation :as animation])

            (flow-gl.graphics [font :as font])))

(def font (font/create "LiberationSans-Regular.ttf" 15))

(def state (atom {}))

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

(defn create-scene-graph [width height]
  (-> (layouts/vertically (animating-text-box "toggle direction"
                                              (fn [] (animation/toggle-direction! :toggle-direction))
                                              {:x (animation/linear-mapping (animation/phase! :toggle-direction
                                                                                              1000)
                                                                            0 50)})
                          (animating-text-box "ping pong once"
                                              (fn [] (animation/start! :ping-pong-once))
                                              {:x (animation/linear-mapping (animation/ping-pong 1
                                                                                                 (animation/phase! :ping-pong-once
                                                                                                                   1000))
                                                                            0 50)})

                          (animating-text-box "infinite ping pong"
                                              (fn [] (animation/toggle! :infinite-ping-pong))
                                              {:x (animation/linear-mapping (animation/ping-pong 1
                                                                                                 (animation/phase! :infinite-ping-pong
                                                                                                                   nil))
                                                                            0 50)})

                          (animating-text-box "key frame"
                                              (fn [] (animation/restart! :key-frame-animation))
                                              {:x (animation/key-frame-mapping (animation/phase! :key-frame-animation
                                                                                                 2000)
                                                                               [0 0
                                                                                0.5 300
                                                                                0.6 250
                                                                                0.7 300
                                                                                0.8 250
                                                                                0.9 300
                                                                                1 0])})
                          
                          (animating-text-box "multi channel key frame"
                                              (fn [] (animation/restart! :multi-channel-key-frame-animation))
                                              (animation/multi-channel-key-frame-mapping (animation/phase! :multi-channel-key-frame-animation
                                                                                                           2000)
                                                                                         
                                                                                         [0 {:x 0 :y 10}
                                                                                          0.2 {:x 300 :y 10}
                                                                                          0.45 {:x 300 :y 100}
                                                                                          0.7 {:x 300 :y 10}
                                                                                          1 {:x 0 :y 10}]))
                          (text-box (str "Frame: " (:frame-number  (swap! state update-in [:frame-number] (fnil inc 0)))))
                          #_(text-box (str "State: " @animation/state-atom)))
      
      (application/do-layout width height)))


(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :animate true)))))


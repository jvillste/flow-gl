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


(defn animating-text-box [text mouse-handler x]
  {:children [(assoc (text-box text)
                     :x x
                     :y 10
                     :mouse-event-handler (fn [node event]
                                            (when (= :mouse-clicked
                                                     (:type event))
                                              (mouse-handler))
                                            event))]
   
   :get-size (fn [node]
               {:width (:available-width node)
                :height 50})})

(defn create-scene-graph [width height]
  (-> (layouts/vertically (animating-text-box "toggle direction"
                                              (fn [] (animation/toggle-direction! :toggle-direction))
                                              (int (animation/linear-mapping (animation/phase! :toggle-direction
                                                                                               3000)
                                                                             0 50)))
                          (animating-text-box "ping pong once"
                                              (fn [] (animation/start! :ping-pong-once))
                                              (int (animation/linear-mapping (animation/ping-pong 1
                                                                                                  (animation/phase! :ping-pong-once
                                                                                                                    2000))
                                                                             0 50)))

                          (animating-text-box "infinite ping pong"
                                              (fn [] (animation/toggle! :infinite-ping-pong))
                                              (int (animation/linear-mapping (animation/ping-pong 1
                                                                                                  (animation/phase! :infinite-ping-pong
                                                                                                                    nil))
                                                                             0 50)))

                          (animating-text-box "keyframe"
                                              (fn [] (animation/toggle! :keyframe-animation))
                                              (animation/key-frame-mapping (animation/ping-pong 3
                                                                                                (animation/phase! :keyframe-animation
                                                                                                                  nil))
                                                                           [0 0
                                                                            0.5 300
                                                                            0.6 250
                                                                            0.7 300
                                                                            0.8 250
                                                                            0.9 300
                                                                            1 250]))
                          (text-box (str "Frame: " (:frame-number  (swap! state update-in [:frame-number] (fnil inc 0)))))
                          (text-box (str "State: " @animation/state-atom)))
 
      (application/do-layout width height)))


(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :animate true)))))


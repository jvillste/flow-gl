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

(defn text-box [color text]
  (assoc layouts/box
         :margin 5
         :children [(visuals/rectangle color
                                       5
                                       5)
                    (visuals/text [0 0 0 255]
                                  font
                                  text)]))

(defn character-editor-keyboard-event-handler [id keyboard-event]
  (case (:type keyboard-event)
    :key-pressed (if (:character keyboard-event)
                   (swap! state assoc-in [id :text] (str (:character keyboard-event))))
    :focus-gained  (swap! state assoc-in [id :has-focus] true)
    :focus-lost  (swap! state assoc-in [id :has-focus] false)
    nil))

(defn character-editor [id]
  (let [editor-state (get @state id)]
    (assoc (text-box (if (:has-focus editor-state)
                       [255 255 255 255]
                       [100 100 100 255])
                     (or (-> editor-state
                             :text)
                         ""))
           :id id
           :keyboard-event-handler (partial character-editor-keyboard-event-handler id)
           :mouse-event-handler keyboard/set-focus-on-mouse-clicked!)))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 100)
  
  (println (animation/time!))
  
  (-> {:children [(assoc (character-editor 1)
                         :x (int (animation/sine 10 20 1 (animation/time!)))
                         :y (int (animation/sine 10 40 2 (animation/time!))))
                  (assoc (character-editor 2)
                         :x 100
                         :y 100)
                  (assoc (character-editor 3)
                         :x 100
                         :y 10)]}
      (application/do-layout width height)))

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :animate true)))))


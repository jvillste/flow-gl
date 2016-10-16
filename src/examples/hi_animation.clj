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

(defn counter-keyboard-event-handler [id keyboard-event]
  (case (:type keyboard-event)
    :key-pressed (swap! state update-in [id :count] (fnil inc 0))
    :focus-gained  (do (println "got focus")
                       (swap! state assoc-in [id :has-focus] true)
                       (animation/reverse! id false)
                       #_(animation/start-animation! id))
    :focus-lost  (do (swap! state assoc-in [id :has-focus] false)
                     (animation/reverse! id true))
    nil))

(defn character-editor [id]
  (let [editor-state (get @state id)]
    (assoc (text-box (if (:has-focus editor-state)
                       [255 255 255 255]
                       [100 100 100 255])
                     (str (or (-> editor-state
                                  :count)
                              0)))
           :id id
           :keyboard-event-handler (partial counter-keyboard-event-handler id)
           :mouse-event-handler keyboard/set-focus-on-mouse-clicked!)))

(def blue [155 155 255 255])

(defn animating-editor [id]
  (let [phase (float (animation/phase! id
                                       (partial animation/linear-phaser 3000)
                                       (partial animation/limit! 0 1)))]
    
    {:children [(assoc (character-editor id)
                       :x (int (animation/linear-mapping phase
                                                         0 50))
                       :y 10)

                (assoc (text-box blue
                                 (pr-str (animation/animation-state @animation/state-atom id)))
                       :x 100
                       :y 10)]}))

(defn create-scene-graph [width height]

  (-> {:children [(assoc (animating-editor 1)
                         :x 10
                         :y 0)
                  (assoc (animating-editor 2)
                         :x 10
                         :y 50)
                  (assoc (animating-editor 3)
                         :x 10
                         :y 100)

                  (assoc (text-box blue (str "Frame: " (:frame-number  (swap! state update-in [:frame-number] (fnil inc 0)))))
                         :x 10
                         :y 150)]}
      (application/do-layout width height)))

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :animate true)))))


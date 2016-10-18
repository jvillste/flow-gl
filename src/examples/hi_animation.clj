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
    :key-pressed (do (swap! state update-in [id :count] (fnil inc 0))
                     (animation/start! [:key-pressed-animation id]))
    :focus-gained  (do (println "got focus")
                       (swap! state assoc-in [id :has-focus] true)
                       (animation/reverse! [:focus-animation id] false))
    :focus-lost  (do (swap! state assoc-in [id :has-focus] false)
                     (animation/reverse! [:focus-animation id] true))
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

#_(partial animation/linear-phaser 2000)
#_(partial animation/limit! 0 1)
#_(fn [runtime]
    (->> (animation/linear-phaser 2000 runtime)
         (animation/limit! 0 1)))

(defn animating-editor [id]
  {:children [(assoc (character-editor id)
                     :x (int (animation/linear-mapping (animation/phase! [:focus-animation id]
                                                                         (partial animation/linear-phaser 2000)
                                                                         (partial animation/limit! 0 1)
                                                                         (fn [phase] phase))
                                                       0 50))
                     :y (+ 10
                           (int (animation/linear-mapping (animation/phase! [:key-pressed-animation id]
                                                                            (partial animation/linear-phaser 70)
                                                                            (partial animation/limit! 0 2)
                                                                            animation/ping-pong)
                                                          0 20))))

              (assoc (text-box blue
                               (str (pr-str (animation/animation-state @animation/state-atom [:key-pressed-animation id]))
                                    (pr-str (animation/animation-state @animation/state-atom [:focus-animation id]))))
                     :x 100
                     :y 10)]})

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
                  (assoc (character-editor 4)
                         :x (+ 10
                               (int (animation/linear-mapping (animation/phase! :looping-animation
                                                                                (partial animation/linear-phaser 1000)
                                                                                (partial animation/no-limit! :looping-animation)
                                                                                animation/ping-pong)
                                                              0 50)))
                         :y 160
                         :mouse-event-handler (fn [node event]
                                                (when (= :mouse-clicked
                                                         (:type event))
                                                  (keyboard/set-focused-node! node)
                                                  (animation/toggle! :looping-animation))
                                                event))

                  (assoc (text-box blue (str "Frame: " (:frame-number  (swap! state update-in [:frame-number] (fnil inc 0)))))
                         :x 10
                         :y 210)]}
      (application/do-layout width height)))

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)

  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph
                                               :animate true)))))


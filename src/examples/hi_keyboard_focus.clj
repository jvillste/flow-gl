(ns examples.hi-keyboard-focus
  (:require [fungl.application :as application]
            (flow-gl.gui [layouts :as layouts]
                         [keyboard :as keyboard]
                         [visuals :as visuals])

            (flow-gl.graphics [font :as font])))

(def font (font/create "LiberationSans-Regular.ttf" 15))

(def state (atom {}))

(defn text-box [color text]
  (layouts/box 5
               (visuals/rectangle color
                                  5
                                  5)
               (visuals/text text
                             [0 0 0 255]
                             35)))

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
  (-> {:children [(assoc (character-editor 1)
                         :x 10
                         :y 10)
                  (assoc (character-editor 2)
                         :x 100
                         :y 100)
                  (assoc (character-editor 3)
                         :x 100
                         :y 10)]}
      (application/do-layout width height)))

(defn start []
  (application/start-window create-scene-graph)
  #_(.start (Thread. (fn []
                       (start-window)))))


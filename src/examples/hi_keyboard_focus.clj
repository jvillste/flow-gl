(ns examples.hi-keyboard-focus
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            (flow-gl.gui [window :as window]
                         [layouts :as layouts]
                         [layout :as layout]
                         [quad-renderer :as quad-renderer]
                         [scene-graph :as scene-graph]
                         [mouse :as mouse]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [events :as events]
                         [component :as component])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target]))
  (:use clojure.test))

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


(defn nodes []
  {:children [(assoc (character-editor 1)
                     :x 10
                     :y 10)
              (assoc (character-editor 2)
                     :x 100
                     :y 100)
              (assoc (character-editor 3)
                     :x 100
                     :y 10)]})


(deftest nodes-test
  (spec-test/instrument)
  (spec/check-asserts true)

  (is (= nil
         (-> (nodes)
             (assoc :available-width 400
                    :available-height 400
                    :x 0
                    :y 0)
             (layout/do-layout)
             #_(scene-graph/leave-nodes)))))

(defn handle-keyboard-event [scene-graph keyboard-event]
  (if (and (= (:key keyboard-event)
              :tab)
           (= (:type keyboard-event)
              :key-pressed))

    (keyboard/move-focus! scene-graph
                          keyboard/order-nodes-down-right
                          (if (:shift keyboard-event)
                            dec
                            inc)
                          keyboard/cycle-position)
    
    (keyboard/call-focused-event-handler keyboard-event)))

(defn start-window []
  (let [window-width 400
        window-height 400
        window (jogl-window/create window-width
                                   window-height
                                   :close-automatically true)
        event-channel (window/event-channel window)]

    (keyboard/with-keyboard-state (keyboard/initialize-keyboard-state)
      (loop [flow-gl-state {:quad-renderer (window/with-gl window gl (quad-renderer/create gl))
                            :mouse-over-state {}}]
        
        (when (window/visible? window)
          (recur (try
                   (let [window-width (window/width window)
                         window-height (window/height window)
                         scene-graph (-> (nodes)
                                         (assoc :x 0
                                                :y 0
                                                :available-width window-width
                                                :available-height window-height)
                                         (layout/do-layout))
                         quad-renderer (window/with-gl window gl
                                         (opengl/clear gl 0 0 0 1)

                                         (quad-renderer/draw (:quad-renderer flow-gl-state)
                                                             (scene-graph/leave-nodes scene-graph)
                                                             window-width
                                                             window-height
                                                             gl))]
                     (window/swap-buffers window)

                     (let [event (async/<!! event-channel)
                           mouse-over-state (if (= :mouse (:source event))
                                              (let [mouse-event-handler-nodes-under-mouse (mouse/mouse-event-handler-nodes-in-coodriantes scene-graph
                                                                                                                                          (:x event)
                                                                                                                                          (:y event))]

                                                (mouse/call-mouse-event-handlers mouse-event-handler-nodes-under-mouse
                                                                                 event)
                                                (mouse/send-mouse-over-events (:mouse-over-state flow-gl-state)
                                                                              mouse-event-handler-nodes-under-mouse))
                                              (:mouse-over-state flow-gl-state))]
                       (when (= :keyboard
                                (:source event))
                         (handle-keyboard-event scene-graph event))
                       
                       {:quad-renderer quad-renderer
                        :mouse-over-state mouse-over-state}))

                   
                   
                   (catch Throwable e
                     (.printStackTrace e *out*)
                     (window/close window)
                     (throw e)))))))
    
    
    (println "exiting")))


(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  (start-window)
  #_(.start (Thread. (fn []
                       (start-window)))))

;; TODO :id can be calculated from :local-id that is only unique among siblings


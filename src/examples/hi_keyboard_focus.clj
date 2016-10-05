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

(defn text-box [color text]
  (assoc layouts/box
         :margin 5
         :children [(visuals/rectangle color
                                       5
                                       5)
                    (assoc layouts/minimum-size
                           :minimum-width 50
                           :minimum-height 0
                           :children [(visuals/text [0 0 0 255]
                                                    font
                                                    text)])]))

(defn handle-text-editor-keyboard-event [editor-state event]
  (cond
    (= :focus-gained
       (:type event))
    (assoc editor-state :has-focus true)

    (= :focus-lost
       (:type event))
    (assoc editor-state :has-focus false)
    
    (events/key-pressed? event :back-space)
    (update-in editor-state [:text] (fn [text]
                                      (apply str (drop-last text))))

    (and (:character event)
         (not= (:key event)
               :enter)
         (= (:type event)
            :key-pressed))
    (update-in editor-state [:text] (fn [text]
                                      (str text
                                           (:character event))))

    :default
    editor-state))

(defn text-editor [id]
  (let [state (component/component-state id)]
    (assoc (text-box (if (:has-focus state)
                       [255 255 255 255]
                       [155 155 155 255])
                     (or (-> state
                             :text)
                         ""))
           :id id
           :keyboard-event-handler handle-text-editor-keyboard-event
           :mouse-event-handler component/give-component-focus-on-mouse-click)))

(defn nodes []
  {:children [(assoc (text-editor 1)
                     :x 0
                     :y 0)

              (assoc (text-editor 2)
                     :x 200
                     :y 0)

              (assoc (text-editor 3)
                     :x 100
                     :y 100)
              
              (assoc (text-editor 4)
                     :x 100
                     :y 200)]})

(defn handle-keyboard-event [scene-graph keyboard-event]
  (if (and (= (:key keyboard-event)
              :tab)
           (= (:type keyboard-event)
              :key-pressed))
    
    (let [next-component-node-infocus (component/next-component-node (component/keyboard-event-handler-nodes-in-down-right-order scene-graph)
                                                                     (component/component-in-focus)
                                                                     (if (:shift keyboard-event)
                                                                       dec
                                                                       inc))]
      
      (component/give-component-focus (:id next-component-node-infocus)
                                      (:keyboard-event-handler next-component-node-infocus)))
    
    (keyboard/call-focused-keyboard-event-handler keyboard-event)))

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

(defn start-window []
  (let [window-width 400
        window-height 400
        window (jogl-window/create window-width
                                   window-height
                                   :close-automatically true)
        event-channel (window/event-channel window)
        keyboard-state (keyboard/initialize-keybaord-state)
        component-state (component/initialize-state)]

    (loop [flow-gl-state {:quad-renderer (window/with-gl window gl (quad-renderer/create gl))
                          :mouse-over-state {}}]
      
      (when (window/visible? window)
        (recur (try
                 (keyboard/with-keyboard-state keyboard-state
                   (component/with-component-state component-state
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
                          :mouse-over-state mouse-over-state}))))
                 
                 
                 (catch Throwable e
                   (.printStackTrace e *out*)
                   (window/close window)
                   (throw e))))))
    
    (println "exiting")))


(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  (start-window)
  #_(.start (Thread. (fn []
                       (start-window)))))

;; visual tree is a scene graph with only child relations
;; layout is a scene graph with coordinates and sizes
;; space = available space
;; size = preferred size

;; abstract visual stree = function from space to visual tree
;; space, abstract visual tree -> children -> size -> layout

;; child function: space -> children
;; size function: space, children -> size
;; space function: space, child sizes -> children's space
;; layout function: space, space functions, size functions, child functions -> child coordinates

;; children depend on available space
;; preferred size depends on children
;; layout depends on preferred size
;; layout is a positioned scene graph
;; preferred-size and adapted-children are calculated during layout because the available space is known only during layout



;; TODO give the node as parameter to mouse event handlers. Then the component id and keyboard event handler can be taken from the nodes :id field.
;; :id can be calculated from :local-id that is only unique among siblings


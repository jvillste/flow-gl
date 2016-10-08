(ns examples.hi-component
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

(defn handle-text-editor-keyboard-event [id event]
  (component/apply-to-component-state! id
                                       (fn [editor-state]
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
                                           editor-state))))

(defn text-editor [id]
  (let [state (component/component-state! id)]
    (assoc (text-box (if (:has-focus state)
                       [255 255 255 255]
                       [155 155 155 255])
                     (or (-> state
                             :text)
                         ""))
           :id id
           :keyboard-event-handler (partial handle-text-editor-keyboard-event id)
           :mouse-event-handler keyboard/set-focus-on-mouse-clicked!)))

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


(defn start-window []
  (let [window-width 400
        window-height 400
        window (jogl-window/create window-width
                                   window-height
                                   :close-automatically true)
        event-channel (window/event-channel window)]

    (with-bindings {#'mouse/state-atom (mouse/initialize-state)
                    #'keyboard/state-atom (keyboard/initialize-state)
                    #'component/state-atom (component/initialize-state)
                    #'quad-renderer/state-atom (window/with-gl window gl (quad-renderer/initialize-state gl))}
      (while (window/visible? window)
        (try
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

                                (quad-renderer/draw! (scene-graph/leave-nodes scene-graph)
                                                     window-width
                                                     window-height
                                                     gl))]
            (window/swap-buffers window)

            (let [event (async/<!! event-channel)]

              (when (= :mouse
                       (:source event))
                (mouse/handle-mouse-event! scene-graph event))
              
              (when (= :keyboard
                       (:source event))
                (keyboard/handle-keyboard-event! scene-graph event))
              
              {:quad-renderer quad-renderer}))

          
          
          (catch Throwable e
            (.printStackTrace e *out*)
            (window/close window)
            (throw e)))))    
    
    
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


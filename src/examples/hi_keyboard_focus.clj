(ns examples.hi-keyboard-focus
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            [clojure.core.async :as async]
            (flow-gl.gui [window :as window]
                         [layouts :as layouts]
                         [layout :as layout]
                         [quad-renderer :as quad-renderer]
                         [scene-graph :as scene-graph]
                         [mouse :as mouse]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [events :as events])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target]))
  (:use flow-gl.utils
        clojure.test))

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
(def state (atom {}))

(defn swap-component-state! [id function & arguments]
  (swap! state
         update-in
         [id]
         (fn [component-state]
           (apply function
                  component-state
                  arguments))))

(defn component-state [id]
  (get @state id))

(defn give-component-focus [component-id component-keyboard-event-handler]
  (do (swap! state assoc :component-in-focus component-id)
      (keyboard/set-focused-keyboard-event-handler (fn [keyboard-event]
                                                     (swap-component-state! component-id
                                                                            component-keyboard-event-handler
                                                                            keyboard-event)))))

(defn give-component-focus-on-mouse-click [component-id component-keyboard-event-handler mouse-event]
  (if (= :mouse-clicked
         (:type mouse-event))
    (give-component-focus component-id component-keyboard-event-handler))

  mouse-event)

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
  (let [state (component-state id)]
    (assoc (text-box (if (:has-focus state)
                       [255 255 255 255]
                       [155 155 155 255])
                     (or (-> state
                             :text)
                         ""))
           :id id
           :component-id id
           :keyboard-event-handler handle-text-editor-keyboard-event
           :mouse-event-handler (partial give-component-focus-on-mouse-click
                                         id
                                         handle-text-editor-keyboard-event))))

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

(defn handle-scene-graph [scene-graph]
  (swap! state assoc :scene-graph scene-graph))

(defn component-nodes-in-down-right-order [scene-graph]
  (->> scene-graph
       (scene-graph/flatten)
       (filter :component-id)
       (sort-by (fn [node] [(:y node) (:x node)]))))

(defn focus-position [component-nodes-in-focus-order component-id]
  (first (positions (fn [node] (= (:component-id node)
                                  component-id))
                    component-nodes-in-focus-order)))

(defn next-component-node-in-focus-chain [advance-function]
  (let [{:keys [component-in-focus scene-graph]} @state
        component-nodes-in-focus-order (vec (component-nodes-in-down-right-order scene-graph))]
    (prn (let [next-position (inc (focus-position component-nodes-in-focus-order component-in-focus))]
           (if (>= next-position (count component-nodes-in-focus-order))
             0
             next-position)))
    (get component-nodes-in-focus-order
         (let [next-position (advance-function (focus-position component-nodes-in-focus-order component-in-focus))]
           (if (>= next-position (count component-nodes-in-focus-order))
             0
             (if (< next-position 0)
               (dec (count component-nodes-in-focus-order))
               next-position))))))

(defn handle-keyboard-event [keyboard-event]
  (if (and (= (:key keyboard-event)
              :tab)
           (= (:type keyboard-event)
              :key-pressed))
    
    (let [component-node (if (:shift keyboard-event)
                           (next-component-node-in-focus-chain dec)
                           (next-component-node-in-focus-chain inc))]
      (give-component-focus (:component-id component-node)
                            (:keyboard-event-handler component-node)))
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
        keyboard-state (keyboard/initialize-keybaord-state)]

    (loop [flow-gl-state {:quad-renderer (window/with-gl window gl (quad-renderer/create gl))
                          :previous-mouse-event-handlers-under-mouse {}}]
      
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

                   (handle-scene-graph scene-graph)

                   (keyboard/with-keyboard-state keyboard-state
                     (let [event (async/<!! event-channel)
                           previous-mouse-event-handlers-by-id (if (= :mouse (:source event))
                                                                 (let [mouse-event-handler-nodes-under-mouse (mouse/mouse-event-handler-nodes-in-coodriantes scene-graph
                                                                                                                                                             (:x event)
                                                                                                                                                             (:y event))]
                                                                   (prn mouse-event-handler-nodes-under-mouse)
                                                                   (mouse/call-mouse-event-handlers mouse-event-handler-nodes-under-mouse
                                                                                                    event)
                                                                   (mouse/send-mouse-over-events (:previous-mouse-event-handlers-under-mouse flow-gl-state)
                                                                                                 mouse-event-handler-nodes-under-mouse))
                                                                 (:previous-mouse-event-handlers-under-mouse flow-gl-state))]
                       (when (= :keyboard
                                (:source event))
                         (handle-keyboard-event event))
                       
                       {:quad-renderer quad-renderer
                        :previous-mouse-event-handlers-under-mouse previous-mouse-event-handlers-by-id})))
                 
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




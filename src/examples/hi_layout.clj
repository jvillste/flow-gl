(ns examples.hi-layout
  (:require [clojure.spec.test :as spec-test]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            [clojure.core.async :as async]
            (flow-gl.gui [window :as window]
                         [layouts :as layouts]
                         [layout :as layout]
                         [quad-renderer :as quad-renderer]
                         [scene-graph :as scene-graph]
                         [mouse :as mouse])

            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 
                                 [render-target :as render-target]))
  (:use flow-gl.utils
        clojure.test))

(defn draw-rectangle [width height color corner-arc-width corner-arc-height]
  (rectangle/create-buffered-image color width height corner-arc-width corner-arc-height))

(defn rectangle [width height color corner-arc-width corner-arc-height]
  {:width width
   :height height
   :image-function draw-rectangle
   :hit-test (fn [x y]
               (rectangle/contains width height corner-arc-width corner-arc-height x y))
   :parameters [width height color corner-arc-width corner-arc-height]})

(def state (atom {}))

(defn stateful-rectangle [id width]
  (-> (rectangle width
                 (if (get-in @state [:mouse-over id])
                   150
                   100)
                 (if (get-in @state [:mouse-over id])
                   (if (get-in @state [:mouse-down id])
                     [255 0 0 150]
                     [255 255 255 150])
                   [0 255 255 150])
                 80 80)
      (assoc :id id
             :mouse-event-handler (fn [event]
                                    (when (contains? #{:on-target nil} (:handling-phase event))
                                      (case (:type event)
                                        :mouse-entered (swap! state assoc-in [:mouse-over id] true)
                                        :mouse-left (swap! state (fn [state]
                                                                   (-> state
                                                                       (assoc-in [:mouse-over id] false)
                                                                       (assoc-in [:mouse-down id] false))))
                                        :mouse-pressed (swap! state assoc-in [:mouse-down id] true)
                                        :mouse-released (swap! state assoc-in [:mouse-down id] false)
                                        nil))
                                    event))))



(def vertical-stack
  {:get-size (fn [node]
               {:width (apply max
                              (conj (map :width (:children node))
                                    0))
                :height (reduce + (map :height (:children node)))})
   
   :give-space (fn [node]
                 (update-in node [:children]
                            (fn [children]

                              (map (fn [child]
                                     (assoc child
                                            :available-width (:available-width node)
                                            :available-height java.lang.Integer/MAX_VALUE))
                                   children))))
   :make-layout (fn [node]
                  (assoc node :children
                         (loop [layouted-nodes []
                                y 0
                                children (:children node)]
                           (if-let [child (first children)] 
                             (recur (conj layouted-nodes
                                          (assoc child
                                                 :x 0
                                                 :y y))
                                    (+ y (:height child))
                                    (rest children))
                             layouted-nodes))))})

(defn nodes []
  (conj {:adapt-to-space (fn [node]
                           (assoc node
                                  :children (loop [index 0
                                                   rectangles []
                                                   y 0]
                                              (let [rectangle (stateful-rectangle index (:available-width node))
                                                    rectangle-size (layout/size rectangle)]
                                                (if (< (+ y (:height rectangle-size))
                                                       (:available-height node))
                                                  (recur (inc index)
                                                         (conj rectangles rectangle)
                                                         (+ y (:height rectangle-size)))
                                                  rectangles)))))}
        vertical-stack))



#_(deftest nodes-test
    (is (= nil
           (-> (nodes)
               (assoc :available-width 400
                      :available-height 400
                      :x 0
                      :y 0)
               (layout/do-layout)
               (scene-graph/leave-nodes)))))


(defn start-window []
  (let [window-width 400
        window-height 400
        window (jogl-window/create window-width
                                   window-height
                                   :close-automatically true)
        event-channel (window/event-channel window)]

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

                   (let [event (async/<!! event-channel)
                         previous-mouse-event-handlers-by-id (if (= :mouse (:source event))
                                                               (let [mouse-event-handler-nodes-under-mouse (mouse/mouse-event-handler-nodes-in-coodriantes scene-graph
                                                                                                                                                           (:x event)
                                                                                                                                                           (:y event))]
                                                                 (mouse/call-mouse-event-handlers mouse-event-handler-nodes-under-mouse
                                                                                                  event)
                                                                 (mouse/send-mouse-over-events (:previous-mouse-event-handlers-under-mouse flow-gl-state)
                                                                                               mouse-event-handler-nodes-under-mouse))
                                                               (:previous-mouse-event-handlers-under-mouse flow-gl-state))]
                     {:quad-renderer quad-renderer
                      :previous-mouse-event-handlers-under-mouse previous-mouse-event-handlers-by-id}))
                 
                 (catch Throwable e
                   (.printStackTrace e *out*)
                   (window/close window)
                   (throw e))))))
    (println "exiting")))


(defn start []
  (spec-test/instrument)
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




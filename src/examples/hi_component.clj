(ns examples.hi-component
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [flow-gl.graphics.text :as text]
            [flow-gl.graphics.rectangle :as rectangle]
            [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            [fungl.application :as application]
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

(defn handle-text-editor-keyboard-event [id text on-change event]
  (cond
    (= :focus-gained
       (:type event))
    (component/apply-to-component-state! id assoc :has-focus true)

    (= :focus-lost
       (:type event))
    (component/apply-to-component-state! id assoc :has-focus false)

    
    (events/key-pressed? event :back-space)
    (on-change (apply str (drop-last text)))

    (and (:character event)
         (not= (:key event)
               :enter)
         (= (:type event)
            :key-pressed))
    (on-change (str text
                    (:character event)))))

(def text-editor
  {:initialize-state (fn [] {:has-focus false})
   :create-scene-graph (fn [state id text on-change]
                         (-> (assoc (text-box (if (:has-focus state)
                                                [255 255 255 255]
                                                [155 155 155 255])
                                              (or text
                                                  ""))
                                    :id id
                                    :mouse-event-handler keyboard/set-focus-on-mouse-clicked!)
                             (keyboard/update-nodes-event-handler! (partial handle-text-editor-keyboard-event
                                                                            id
                                                                            text
                                                                            on-change))))})

(def root
  {:initialize-state (fn []
                       {:text-1 "foo"
                        :text-2 "bar"
                        :text-3 "baz"})
   
   :create-scene-graph (fn [state id]
                         (assoc layouts/vertical-stack
                                :children [(component/call! text-editor
                                                            (conj id :text-editor-1)
                                                            (:text-1 state)
                                                            (fn [new-text]
                                                              (component/apply-to-component-state! id assoc :text-1 new-text)))
                                           (component/call! text-editor
                                                            (conj id :text-editor-2)
                                                            (:text-2 state)
                                                            (fn [new-text]
                                                              (component/apply-to-component-state! id assoc :text-2 new-text)))
                                           (text-box [255 255 255 255]
                                                     (pr-str state))]))})



(defn create-scene-graph [width height]
  (-> (component/call! root [:root])
      (application/do-layout width height)))

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  (application/start-window create-scene-graph)
  #_(.start (Thread. (fn []
                       (start-window)))))



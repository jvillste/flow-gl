(ns flow-gl.gui.components.text-editor
  (:require [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [events :as events])

            (flow-gl.graphics [font :as font]))
  
  (:use midje.sweet))

(defn append-character [string character]
  (apply str (vec (concat string
                          (str character)))))

(fact (append-character "Foo" \a)
      => "Fooa")

(defn handle-text-editor-event [state event]
  (if (:editing? state)
    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :text (:edited-text state))
         (assoc :editing? false))

     (events/key-pressed? event :back-space)
     (let [new-text (apply str (drop-last (:edited-text state)))]
       (when (:on-change state)
         ((:on-change state) new-text))
       (assoc-in state [:edited-text] new-text))

     (and (:character event)
          (= (:type event)
             :key-pressed))
     (let [new-text (append-character (:edited-text state)
                                      (:character event))]
       (when (:on-change state)
         ((:on-change state) new-text))
       (assoc-in state [:edited-text] new-text))

     :default
     state)

    (cond

     (events/key-pressed? event :enter)
     (-> state
         (assoc :edited-text (:text state))
         (assoc :editing? true))


     :default
     state)))

(def initial-text-editor-state
  {:text ""
   :edited-text ""
   :editing? false
   :has-focus false
   :handle-keyboard-event handle-text-editor-event})

(defn text-editor-view [state]
  [state
   (layout/->Box 10 [(drawable/->Rectangle 0
                                           0
                                           (if (:has-focus state)
                                             [0 0.8 0.8 1]
                                             [0 0.5 0.5 1]))
                     (drawable/->Text (if (:editing? state)
                                        (:edited-text state)
                                        (:text state))
                                      (font/create "LiberationSans-Regular.ttf" 15)
                                      (if (:has-focus state)
                                        (if (:editing? state)
                                          [0 0 1 1]
                                          [0 0 0 1])
                                        [0.3 0.3 0.3 1]))])])

(def text-editor {:initial-state initial-text-editor-state
                  :view text-editor-view})

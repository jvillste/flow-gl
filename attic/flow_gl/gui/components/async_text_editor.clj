(ns flow-gl.gui.components.async-text-editor
  (:require [clojure.core.async :as async]
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image])
            (flow-gl.gui [events :as events]
                         [layout-dsl :as l]
                         [drawable :as drawable]
                         [gui :as gui])))

(defn handle-new-text [state new-text]
  (when (:on-change state)
    (async/go (async/>! (:on-change state) new-text)))
  (assoc-in state [:text] new-text))

(defn handle-text-editor-event [state event]
  
  (cond
    (events/key-pressed? event :back-space)
    (handle-new-text state (apply str (drop-last (:text state))))

    (and (:character event)
         (= (:type event)
            :key-pressed))
    (handle-new-text state (str (:text state)
                                (:character event)))

    :default
    state))



(defn text-editor-view [view-context state]
  (l/box 10
         (drawable/->Rectangle 0
                               0
                               (cond
                                 (:has-focus state) [255 255 255 255]
                                 (:mouse-over state) [250 250 250 255]
                                 :default [250 250 250 255]))
         (drawable/->Text (:text state)
                          (font/create "LiberationSans-Regular.ttf" 15)
                          [0 0 0 255])))

(defn text-editor [view-context]
  {:local-state {:text ""}
   :view #'text-editor-view
   :handle-keyboard-event (fn [state event]
                            (gui/apply-to-local-state state view-context handle-text-editor-event event))
   :can-gain-focus true})

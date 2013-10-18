(ns flow-gl.gui.reset-view
  (:require [flow-gl.gui.view :as view]))

(def state-atom-atom (atom nil))

(defn initialize [state state-atom]
  (reset! state-atom-atom state-atom)
  state)

(defn reset-view [view]
  (when @state-atom-atom
    (swap! @state-atom-atom view/set-view view)))

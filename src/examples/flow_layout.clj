(ns examples.flow-layout
  (:require [clojure.core.async :as async]
            [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events]
                         [view :as view])

            (flow-gl.gui.components [text-editor :as text-editor]
                                    [button :as button])

            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]))
  (:use flow-gl.utils
        midje.sweet
        flow-gl.gui.layout-dsl))

(view/def-view todo-list [state]
  {:todo-texts []
   :new-todo-text ""
   :handle-keyboard-event (fn [state event]
                            (cond
                             (events/key-pressed? event :space)
                             (update-in state [:todo-texts] insert 0 "New item")

                             (events/key-pressed? event :back-space)
                             (update-in state [:todo-texts] remove-nth 0)

                             (events/key-pressed? event :esc)
                             (assoc state :close-requested true)

                             :default
                             state))}

  (apply vertically (concat (for-all [[index todo-text] (indexed (:todo-texts state))]
                                     (view/call-view [:todo-text-editor index]
                                                     text-editor/text-editor
                                                     {:text todo-text
                                                      :on-change (view/apply-to-current-state [state new-text]
                                                                                              (assoc-in state [:todo-texts index] new-text))}))

                            [(margin
                              10 0 0 0
                              (horizontally
                               (view/call-view
                                :new-todo-text
                                text-editor/text-editor
                                {:text (:new-todo-text state)
                                 :on-change (view/apply-to-current-state [state new-text]
                                                                         (assoc state :new-todo-text new-text))})
                               (margin 0 0 0 10
                                       (view/call-view :add-new
                                                       button/button
                                                       {:text "Add new"
                                                        :on-click (view/apply-to-current-state [state]
                                                                                               (-> state
                                                                                                   (update-in [:todo-texts] conj (:new-todo-text state))
                                                                                                   (assoc :new-todo-text "")))}))))])))


(def event-queue (atom (event-queue/create)))

(defn start []
  (reset! event-queue (event-queue/create))
  (.start (Thread. (fn [] (view/start-view @event-queue
                                           todo-list)))))

(event-queue/add-event @event-queue {})

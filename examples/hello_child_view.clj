(ns examples.hello-child-view
  (:require [flow-gl.utils :as utils]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [event-queue :as event-queue]
                         [events :as events])

            (flow-gl.graphics [command :as command]
                              [font :as font])

            (flow-gl.graphics.command [text :as text]
                                      [translate :as translate])

            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as window]))
  (:use [flow-gl.utils]))

(defn start-view [view event-handler initial-state]
  (let [event-queue (event-queue/create)
        window (window/create 300
                              300
                              opengl/initialize
                              opengl/resize
                              event-queue)]

    (try
      (loop [state initial-state]
        (println state)
        (let [layoutable (view state)]
          (window/render window gl
                         (opengl/clear gl 0 0 0 1)
                         (doseq [command (drawable/drawing-commands (layout/layout layoutable
                                                                                   (window/width window)
                                                                                   (window/height window)))]
                           (doto (command/create-runner command gl)
                             (command/run gl)
                             (command/delete gl)))))

        (let [event (event-queue/dequeue-event-or-wait event-queue)]
          (if (= (:type event)
                 :close-requested)
            (window/close window)

            (recur (event-handler state event)))))

      (catch Exception e
        (window/close window)
        (throw e)))))


(defn handle-hello-event [state event]
  (cond (events/key-pressed? event :enter)
        (update-in state [:count] inc)

        :default state))

(defn hello-view [state]
  (drawable/->Text (str "Hello " (:count state))
                   (font/create "LiberationSans-Regular.ttf" 40)
                   (if (:in-focus state)
                     [1 1 1 1]
                     [0.7 0.7 0.7 1])))


(defn view [state]
  (layout/->VerticalStack (concat (map-indexed (fn [index child-state]
                                                     (hello-view (conj child-state
                                                                       {:in-focus (= index
                                                                                     (:focus state))})))
                                                   (:children state))

                                  [(drawable/->Text (str "Sum " (reduce + (map :count (:children state))))
                                                    (font/create "LiberationSans-Regular.ttf" 40)
                                                    [1 1 1 1])])))

(defn handle-event [state event]
  (cond  (events/key-pressed? event :down)
         (update-in state [:focus] (fn [focus]
                                     (if (= focus
                                            (dec (count (:children state))))
                                       0
                                       (inc focus))))


         :default
         (update-in state
                    [:children (:focus state)]
                    (fn [hello-state]
                      (handle-hello-event hello-state event)))))

(defn start []
  (start-view view
              handle-event
              {:focus 0
               :children [{:count 0}
                          {:count 0}]}))

;;(start)

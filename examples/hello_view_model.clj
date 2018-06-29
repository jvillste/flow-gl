(ns examples.hello-view-model
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
  (:use flow-gl.utils
        midje.sweet))

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

(def initial-hello-view-state {:count 0
                               :in-focus false})

(defn hello-view [state]
  (drawable/->Text (str "Hello " (:count state))
                   (font/create "LiberationSans-Regular.ttf" 40)
                   (if (:in-focus state)
                     [1 1 1 1]
                     [0.7 0.7 0.7 1])))

(defn hello-view-state-to-view-model [state]
  (:count state))

(defn hello-view-model-to-view-state [model state]
  (assoc state :count model))

(def initial-view-state {:focus 0})

(defn view-state-to-view-model [state]
  (map :count (:children state)))

(defn view-model-to-view-state [model state]
  (assoc state :children (vec (map (fn [[hello-view-model hello-view-state]]
                                     (hello-view-model-to-view-state hello-view-model hello-view-state))
                                   (partition 2 (interleave model
                                                            (or (:children state)
                                                                (repeat (count model) initial-hello-view-state))))))))

(fact (view-model-to-view-state [0 0] {:focus 0})
      => {:children [{:count 0, :in-focus false} {:count 0, :in-focus false}], :focus 0})


(defn view [state]
  (layout/->VerticalStack (concat (map hello-view
                                       (:children state))

                                  [(drawable/->Text (str "Sum " (reduce + (view-state-to-view-model state)))
                                                    (font/create "LiberationSans-Regular.ttf" 40)
                                                    [1 1 1 1])])))

(defn handle-event [state event]
  (cond  (events/key-pressed? event :down)
         (let [old-focus (:focus state)
               new-focus (if (= old-focus
                                (dec (count (:children state))))
                           0
                           (inc old-focus))]
           (-> (assoc-in state [:focus] new-focus)
               (assoc-in [:children] (vec (map-indexed (fn [index child-state]
                                                         (assoc child-state
                                                           :in-focus (= index  new-focus)))
                                                       (:children state))))))

         :default
         (update-in state
                    [:children (:focus state)]
                    (fn [hello-state]
                      (handle-hello-event hello-state event)))))

(defn start []
  (start-view view
              handle-event
              (view-model-to-view-state [1 2] initial-view-state)))

;;(start)

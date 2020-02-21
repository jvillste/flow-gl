(ns examples.todo
  (:require [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layouts :as layouts]))

(def initial-state {:todos [{:id 1
                             :text "Do this"}
                            {:id 2
                             :text "Do that"}
                            {:id 3
                             :text "Do this and that"}]})

(defn todo-view []
  (println "todo-view")
  (let [state-atom (atom-registry/get! :todo-state {:create (constantly initial-state)})]
    (layouts/vertically (for [todo (:todos @state-atom)]
                          (assoc (visuals/text (:text todo))
                                 :mouse-event-handler (fn [node event]
                                                        (prn (:type event)
                                                             #_node)
                                                        event))))))

(defn create-scene-graph [width height]
  (animation/swap-state! animation/set-wake-up 1000)
  (application/do-layout (todo-view)
                         width
                         height))


(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))

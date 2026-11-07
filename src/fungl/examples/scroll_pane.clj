(ns fungl.examples.scroll-pane
  (:require
   [flow-gl.graphics.font :as font]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]))

(def log-atom (atom []))

(defn log! [message]
  (println message)
  (swap! log-atom conj message))

(defn scroll-pane [_content]
  (let [state-atom (dependable-atom/atom {:x -100 :y 0})]
    (fn [content]
      (layouts/superimpose (visuals/rectangle-2 {:fill-color [0 100 188 255]})
                           (assoc (visuals/clip (layouts/transpose (:x @state-atom)
                                                                   (:y @state-atom)
                                                                   content))
                                  :width 500
                                  :height 500
                                  :mouse-event-handler (fn [_node event]
                                                         (when (and (= :mouse-clicked (:type event))
                                                                    (or (= :going-down (:handling-phase event))
                                                                        (= :on-target (:handling-phase event))))

                                                           (log! "scroll pane clicked"))

                                                         (when (= :mouse-wheel-rotated
                                                                  (:type event))
                                                           (swap! state-atom
                                                                  update
                                                                  (if (:horizontal? event)
                                                                    :x :y)
                                                                  (fn [value]
                                                                    (- value (* 10 (:precise-wheel-rotation event))))))

                                                         event))))))

(defn text-view []
  (layouts/with-maximum-size 250 nil
    {:node (visuals/text-area "hello asdf asdf asdf dsaf asd fsa")
     :mouse-event-handler (fn [_node event]
                            (when (= :mouse-clicked (:type event))
                              (println "handler 1" (:handling-phase event)))
                            event)}))



(defn root-view []
  {:node [scroll-pane (layouts/vertically-2 {:margin 20}
                                            (text-view)
                                            (assoc (visuals/rectangle-2)
                                                   :width 100
                                                   :height 100
                                                   :mouse-event-handler (fn [_node event]
                                                                          (when (= :mouse-clicked (:type event))
                                                                            (log! "child clicked"))
                                                                          event))
                                            ;; (text-view)
                                            )]
   :width 500
   :height 500
   :x 200
   :y 200})

(defn root-view-2 []
  (assoc (layouts/vertically-2 {:margin 10}
                               [root-view]
                               [root-view]                        )
         :x 200
         :y 200))

(application/def-start root-view)

(comment
  (when (deref event-channel-atom)
    (clojure.core.async/>!! (deref event-channel-atom) {:type :reset}))
  )

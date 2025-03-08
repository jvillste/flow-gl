(ns fungl.examples.state
  (:require
   [clojure.core.async :as async]
   [flow-gl.gui.keyboard :as keyboard]
   [flow-gl.gui.visuals :as visuals]
   [fungl.application :as application]
   [fungl.component.text-area :as text-area]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.layouts :as layouts]))

;; TODO: does the cache work here? in times table id does not

(defn counter []
  (let [count-atom (dependable-atom/atom "count"
                                         0)]
    (fn view []
      {:node (text-area/text (str "count " @count-atom)
                             (if (keyboard/component-is-in-focus?)
                               [255 255 255 255]
                               [180 180 180 255]))
       :can-gain-focus? true
       :keyboard-event-handler (fn [_scene-graph event]
                                 (when (keyboard/key-pressed? event :f)
                                   (println "incrementing count" @count-atom)
                                   (swap! count-atom inc)))})))

(defn state-demo []
  (println "state-demo constructor")
  (let [count-atom (dependable-atom/atom "view-count"
                                         1)]
    (fn view []
      (keyboard/sub-component-is-in-focus?)
      (assoc (layouts/horizontally-2 {:margin 20}
                                     (for [index (range @count-atom)]
                                       {:node [counter]
                                        :local-id (str "counter " index)}))
             :keyboard-event-handler (fn [_scene-graph event]
                                       (when (keyboard/descendant-key-pressed? event :d)
                                         (keyboard/cycle-focus (:scene-graph @keyboard/state-atom true)))

                                       (when (keyboard/descendant-key-pressed? event :s)
                                         (keyboard/cycle-focus (:scene-graph @keyboard/state-atom true)
                                                               false))

                                       (when (keyboard/descendant-key-pressed? event :g)
                                         (println "incrementing component count")
                                         (swap! count-atom inc))

                                       (when (keyboard/descendant-key-pressed? event :a)
                                         (println "decrementing component count")
                                         (swap! count-atom (fn [count]
                                                             (max 1 (dec count)))))

                                       (when (keyboard/descendant-key-pressed? event :escape)
                                         (application/send-event! {:type :close-requested})))))))

(defn root []
  (layouts/superimpose (visuals/rectangle [0 0 0 255] 0 0)
                       (layouts/vertically-2 {:margin 10}
                                             [state-demo]
                                             [state-demo])))

(def event-channel-atom (atom nil))

(defn start []
  (reset! event-channel-atom
          (application/start-application #'root)))

(when @event-channel-atom
  (async/>!! @event-channel-atom
             {:type :redraw}))

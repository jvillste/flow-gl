(ns examples.hi-stateful
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (flow-gl.gui [layouts :as layouts]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [stateful :as stateful]
                         [events :as events])

            (flow-gl.graphics [font :as font])))

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
    (stateful/apply-to-stateful-state! id assoc :has-focus true)

    (= :focus-lost
       (:type event))
    (stateful/apply-to-stateful-state! id assoc :has-focus false)

    
    (events/key-pressed? event :back-space)
    (on-change (apply str (drop-last text)))

    (and (:character event)
         (not= (:key event)
               :enter)
         (= (:type event)
            :key-pressed))
    (on-change (str text
                    (:character event)))))

(defn text-editor [id text on-change]
  (stateful/call-with-state! id

                             [text
                              on-change]
                             
                             (fn [] {:has-focus false})
                             
                             (fn [state id text on-change]
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
                                                                                  on-change))))))

(defn root [id]
  (stateful/call-with-state! id

                             []
                             
                             (fn []
                               {:text-1 "foo"
                                :text-2 "bar"})
                             
                             (fn [state id]
                               (assoc layouts/vertical-stack
                                      :children [(text-editor (conj id :text-editor-1)
                                                              (:text-1 state)
                                                              (fn [new-text]
                                                                (stateful/apply-to-stateful-state! id assoc :text-1 new-text)))
                                                 (text-editor (conj id :text-editor-2)
                                                              (:text-2 state)
                                                              (fn [new-text]
                                                                (stateful/apply-to-stateful-state! id assoc :text-2 new-text)))

                                                 (text-box [255 255 255 255]
                                                           (pr-str state))]))))



(defn create-scene-graph [width height]
  (-> (root [:root])
      (application/do-layout width height)))

(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  (application/start-window create-scene-graph)
  #_(.start (Thread. (fn []
                       (start-window)))))



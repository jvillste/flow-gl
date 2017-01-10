(ns examples.hi-atom-registry
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            [fungl.application :as application]
            (fungl [cache :as cache]
                   [callable :as callable]
                   [atom-registry :as atom-registry]
                   [value-registry :as value-registry])
            (flow-gl.gui [layouts :as layouts]
                         [keyboard :as keyboard]
                         [visuals :as visuals]
                         [stateful :as stateful]
                         [events :as events])

            (flow-gl.graphics [font :as font]))
  (:use [clojure.test]))

(def font (font/create "LiberationSans-Regular.ttf" 15))

(defn text-box [color text]
  (layouts/with-minimum-size 50 0
    (layouts/box 5
                 (visuals/rectangle color
                                    5
                                    5)
                 (visuals/text [0 0 0 255]
                               font
                               text))))

(defn handle-text-editor-keyboard-event [state-atom text on-change event]
  (cond
    (= :focus-gained
       (:type event))
    (swap! state-atom assoc :has-focus true)

    (= :focus-lost
       (:type event))
    (swap! state-atom assoc :has-focus false)

    
    (events/key-pressed? event :back-space)
    (callable/call on-change (apply str (drop-last text)))

    (and (:character event)
         (not= (:key event)
               :enter)
         (= (:type event)
            :key-pressed))
    (callable/call on-change (str text
                                  (:character event)))))

(defn text-editor [id text on-change]
  (println "text editor")
  (let [state-atom (atom-registry/get! id {:create (fn [] {:has-focus false})})]
    (-> (assoc (text-box (if (:has-focus @state-atom)
                           [255 255 255 255]
                           [155 155 155 255])
                         (or text
                             ""))
               :id id
               :mouse-event-handler keyboard/set-focus-on-mouse-clicked!)
        (keyboard/update-nodes-event-handler! (partial handle-text-editor-keyboard-event
                                                       state-atom
                                                       text
                                                       on-change)))))


(defn on-text-change [state-atom key new-text]
  (swap! state-atom assoc key new-text))

(defn root [id]
  (let [state-atom (atom-registry/get! id {:create (fn [] {:text-1 "foo"
                                                           :text-2 "bar"})})]
    (println "root")
    (layouts/vertically (cache/call! text-editor
                                     [id :text-editor-1]
                                     (:text-1 @state-atom)
                                     [on-text-change state-atom :text-1])
                        (cache/call! text-editor
                                     [id :text-editor-2]
                                     (:text-2 @state-atom)
                                     [on-text-change state-atom :text-2])

                        (text-box [255 255 255 255]
                                  (pr-str @state-atom)))))


(defn create-scene-graph [width height]
  (-> (cache/call! root :root)
      (application/do-layout width height)))



(deftest cache-test
  (with-bindings (application/create-event-handling-state)
    (is (= 1 @(atom-registry/get! :baz {:create (fn [] 1)}))))
  
  
  (let [foo-call-count (atom 0)
        bar-call-count (atom 0)
        value-specification {:create (fn [] 1)}
        bar (fn [x]
              (let [bar-atom (atom-registry/get! :bar value-specification)]
                (swap! bar-call-count inc)
                (+ x @bar-atom)))
        foo (fn [x]
              (let [foo-atom (atom-registry/get! :foo value-specification)]
                (swap! foo-call-count inc)
                (+ @foo-atom
                   (bar x))))]
    (println "--------------------")
    (with-bindings (application/create-event-handling-state)
      (is (= 0
             @foo-call-count))
      (is (= 12
             (cache/call! foo 10)))
      (is (= 1
             @foo-call-count))
      
      (is (= 12
             (cache/call! foo 10)))
      (is (= 1
             @foo-call-count))

      (is (= 7
             (cache/call! foo 5)))
      (is (= 2
             @foo-call-count))
      (reset! (atom-registry/get! :bar value-specification)
              20)
      (is (= 26
             (cache/call! foo 5)))

      (value-registry/delete-unused-values! 0)
      (is (= 26
             (cache/call! foo 5)))
      (is (= 3
             @foo-call-count))

      (value-registry/delete-unused-values! -1)
      (value-registry/delete-unused-values! -1)
      (is (= 7
             (cache/call! foo 5)))
      (is (= 4
             @foo-call-count)))))


(defn start []
  (spec-test/instrument)
  (spec/check-asserts true)
  #_(application/start-window create-scene-graph)
  (.start (Thread. (fn []
                     (application/start-window create-scene-graph)))))



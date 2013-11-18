(ns flow-gl.gui.focus
  (:require  [flow-gl.dataflow :as dataflow]
             [flow-gl.gui.awt-input :as input])
  (:use clojure.test))

(defn get-child-event-handler [state child]
  (get (dataflow/get-value-from state :child-event-handlers) child))

(defn set-focusable-children [& names]
  (dataflow/define :focusable-children names)
  (dataflow/define :child-in-focus (first names))
  (dataflow/define [(first names) :has-focus] true))

(defn set-focusable-children [& names-and-event-handlers]
  (let [event-handlers-map (apply hash-map names-and-event-handlers)
        names (keys event-handlers-map)]
    (dataflow/define :child-event-handlers event-handlers-map)
    (dataflow/define :focusable-children names)
    (dataflow/initialize :child-in-focus (first names))
    (dataflow/define [(first names) :has-focus] true)))

(defn view-path-in-focus [parent-path]
  (if-let [child-in-focus-key (dataflow/maybe-get-global-value (dataflow/path parent-path :child-in-focus))]
    (recur (dataflow/path parent-path child-in-focus-key))
    parent-path))

(defn next-in-focus [in-focus focusables]
  (loop [next-focusables focusables]
    (if (seq next-focusables)
      (if (= in-focus
             (first next-focusables))
        (or (second next-focusables)
            (first focusables))
        (recur (rest next-focusables)))
      nil)))

(deftest next-in-focus-test
  (are  [focusables in-focus result] (= (next-in-focus in-focus focusables)
                                        result)

        [:a :b :c] :b :c
        [:a :b :c] :c :a
        [:a :b :c] :a :b
        [:a :b :c] :e nil))

(defn move-focus [state]
  (let [was-in-focus (dataflow/get-value-from state :child-in-focus)
        now-in-focus (next-in-focus was-in-focus
                                    (dataflow/get-value-from state :focusable-children))]
    (-> state
        (dataflow/define-to [was-in-focus :has-focus] false)
        (dataflow/define-to [now-in-focus :has-focus] true)
        (dataflow/define-to :child-in-focus now-in-focus)
        (as-> state
              (let [handler (get-child-event-handler state now-in-focus)]
                (binding [dataflow/current-path (dataflow/path dataflow/current-path now-in-focus)]
                  (handler state {:type :focus-gained})))))))


#_(deftest move-focus-test
    (is (= (-> (dataflow/create)
               (dataflow/define-to [:view :child-in-focus] :child1)
               (dataflow/define-to [:view :focusable-children] [:child1 :child2])
               (move-focus [:view])
               (move-focus [:view])
               (dataflow/to-map))

           {[:view :child-in-focus] :child1
            [:view :child1 :has-focus] true
            [:view :child2 :has-focus] false
            [:view :focusable-children] [:child1 :child2]})))

(defn pass-event-to-focused-child [state event]
  (let [child-in-focus (dataflow/get-value-from state :child-in-focus)
        handler (get-child-event-handler state child-in-focus)]
    (binding [dataflow/current-path (dataflow/path dataflow/current-path child-in-focus)]
      (handler state event))))

(ns examples.boxes.core
  (:require (flow-gl.gui [awt-input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow])
            [examples.boxes.box :as box])
  (:use clojure.test))

(defn set-focusable-children [& names]
  (dataflow/define :focusable-children names)
  (dataflow/define :child-in-focus (first names))
  (dataflow/define [(first names) :has-focus] true))


(defn view []
  (set-focusable-children :box1 :box2 :box3)
  (layout/->Stack [(view/init-and-call :box1 box/view)
                   (view/init-and-call :box2 box/view)
                   (view/init-and-call :box3 box/view)]))

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

(defn move-focus [state parent]
  (let [was-in-focus (get state (dataflow/path parent :child-in-focus))
        now-in-focus (next-in-focus was-in-focus
                                    (get state (dataflow/path parent :focusable-children)))]
    (-> state
        (dataflow/define-to (dataflow/path parent was-in-focus :has-focus) false)
        (dataflow/define-to (dataflow/path parent now-in-focus :has-focus) true)
        (dataflow/define-to (dataflow/path parent :child-in-focus) now-in-focus))))

(deftest move-focus-test
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

(defn handle-event [state view event]
  (cond (input/key-pressed? event input/tab)
        (move-focus state view)

        (input/key-pressed? event input/esc)
        (do (application/request-close)
            state)

        :default
        (box/handle-event state
                          (dataflow/path view (get state (dataflow/path view :child-in-focus)))
                          event)))

(defn initialize [state state-atom]
  state)

(defn start []
  (application/start view
                     :handle-event handle-event
                     :initialize initialize
                     :framerate 60))

(defn refresh []
  (when @application/state-atom-atom
    (swap! @application/state-atom-atom
           view/set-view
           view)))

(refresh)

(defn start-async []
  (.start (Thread. start)))

(comment
  (start-async)
  (start)
  )

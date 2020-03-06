(ns examples.view-compiler
  (:require [fungl.application :as application]
            [fungl.component.button :as button]
            [fungl.dependable-atom :as dependable-atom]
            [fungl.layouts :as layouts]))

(defn counter-view [number_]
  (let [count-atom (dependable-atom/atom 0)]
    (fn [number]
      (button/button (str "count " number "=" @count-atom)
                     (fn [] (swap! count-atom inc))))))

(defn view []
  (let [counter-numbers-atom (dependable-atom/atom #{1 2 3})]
    (fn []
      (layouts/vertically-2 {:margin 10}
                            (for [counter-number (sort @counter-numbers-atom)]
                              (assoc (layouts/horizontally-2 {:margin 10}
                                                             [counter-view counter-number]
                                                             (button/button (str "remove " counter-number)
                                                                            (fn [] (swap! counter-numbers-atom
                                                                                          disj
                                                                                          counter-number))))
                                     :id counter-number))))))

(defn start []
  (application/start-window #'view))

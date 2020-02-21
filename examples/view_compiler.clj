(ns view-compiler
  (:require [fungl.view-compiler :as view-compiler]
            [fungl.layouts :as layouts]
            [fungl.cache :as cache]
            [fungl.component.text-area :as text-area]
            [fungl.component.button :as button]))

(defn counter-view [number]
  (let [count-atom (atom 0)]
    (button/default-button (str "count " number ":" @count-atom)
                           (fn [] (swap! count-atom inc)))))

(defn root-view []
  (let [counter-numbers-atom (atom #{1 2 3})]
    (fn [] (layouts/vertically-2 {:margin 10}
                                 (for [counter-number @counter-numbers-atom]
                                   (layouts/horizontally-2 {:margin 10}
                                                           [counter-view counter-number]
                                                           (button/default-button (str "remove " counter-number)
                                                                                  (fn [] (swap! counter-numbers-atom
                                                                                                disj
                                                                                                counter-number)))))))))

(comment
  (root-view)

  (meta ^:foo [])

  (with-bindings (merge (cache/state-bindings)
                        (view-compiler/state-bindings))
    (view-compiler/compile [root-view]))
  ) ;; TODO: remove-me

(comment

  ) ;; TODO: remove-me

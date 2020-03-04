(ns examples.view-compiler
  (:require [fungl.view-compiler :as view-compiler]
            [fungl.layouts :as layouts]
            [fungl.cache :as cache]
            [fungl.component.text-area :as text-area]
            [fungl.component.button :as button]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [flow-gl.graphics.font :as font]
            [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.atom-registry :as atom-registry]
            [fungl.callable :as callable]
            [fungl.component.text-area :as text-area]
            [fungl.layouts :as layouts]

            [clojure.test :refer :all]

            [flow-gl.opengl.jogl.window :as jogl-window]

            [clojure.test :refer [deftest]]
            [flow-gl.gui.keyboard :as keyboard]))

(defn counter-view [number_]
  (let [count-atom (dependable-atom/atom 0)]
    (fn [number]
      (button/default-button (str "count " number ":" @count-atom)
                             (fn [] (swap! count-atom inc))))))

(defn root-view []
  (let [counter-numbers-atom (dependable-atom/atom #{1 2 3})]
    (fn [] (layouts/vertically-2 {:margin 10}
                                 (for [counter-number @counter-numbers-atom]
                                   (layouts/horizontally-2 {:margin 10}
                                                           ^:id counter-number [counter-view counter-number]
                                                           (button/default-button (str "remove " counter-number)
                                                                                  (fn [] (swap! counter-numbers-atom
                                                                                                disj
                                                                                                counter-number)))))))))

(defn start []
  (application/start-window (button/default-button "hello" (fn [] ))
                            :window
                            (jogl-window/create 600 1000
                                                :close-automatically true)))

(comment
  (root-view)

  (meta ^:foo [])

  (with-bindings (merge (cache/state-bindings)
                        (view-compiler/state-bindings))
    (view-compiler/compile [root-view]))
  ) ;; TODO: remove-me

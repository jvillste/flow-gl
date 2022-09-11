(ns fungl.dependable-atom-test
  (:require (fungl [cache :as cache]
                    [dependable-atom :as dependable-atom]))
  (:use [clojure.test]))

(deftest cache-test
  (with-bindings (cache/state-bindings)
    (let [state-atom (dependable-atom/atom 0)
          call-count-atom (atom 0)
          function (fn []
                     (swap! call-count-atom inc)
                     @state-atom)]
      (is (= 0 @call-count-atom))

      (is (= 0 (cache/call! function)))
      (is (= 1 @call-count-atom))

      (is (= 0 (cache/call! function)))
      (is (= 1 @call-count-atom))

      (swap! state-atom inc)

      (is (= 1 (cache/call! function)))
      (is (= 2 @call-count-atom))

      (is (= 1 (cache/call! function)))
      (is (= 2 @call-count-atom)))))

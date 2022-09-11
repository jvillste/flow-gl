(ns fungl.dependable-atom-test
  (:require
   [clojure.test :refer :all]
   [fungl.cache :as cache]
   [fungl.dependable-atom :as dependable-atom]
   [fungl.derivation :as derivation]))

(deftest derivation-test
  (with-bindings (cache/state-bindings)
    (let [state-atom (dependable-atom/atom "state-atom" 1)
          derivation-call-count-atom (atom 0)
          derivation (derivation/derive "derivation"
                                        (fn []
                                          (swap! derivation-call-count-atom inc)
                                          (even? @state-atom)))

          cached-function-call-count-atom (atom 0)
          cached-function (fn cached-function []
                            (swap! cached-function-call-count-atom inc)
                            @derivation)]

      (is (= false (cache/call! cached-function)))

      (is (= 1 @derivation-call-count-atom))
      (is (= 1 @cached-function-call-count-atom))

      (reset! state-atom 1)

      (is (= false (cache/call! cached-function)))

      (is (= 1 @derivation-call-count-atom))
      (is (= 1 @cached-function-call-count-atom))

      (reset! state-atom 3)

      (is (= false (cache/call! cached-function)))

      (is (= 2 @derivation-call-count-atom))
      (is (= 1 @cached-function-call-count-atom))


      (reset! state-atom 2)

      (is (= true (cache/call! cached-function)))

      (is (= 3 @derivation-call-count-atom))
      (is (= 2 @cached-function-call-count-atom)))))

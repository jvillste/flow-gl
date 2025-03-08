(ns fungl.derivation-test
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


(deftest identity-test
  (with-bindings (cache/state-bindings)
    (let [foo-atom (dependable-atom/atom {:a 1
                                          :b 2})
          bar-derivation (derivation/derive (fn []
                                              #{(:b @foo-atom)}))]

      (is (identical? @bar-derivation
                      @bar-derivation))

      (let [bar-state @bar-derivation]
        (swap! foo-atom update :a inc)
        (is (identical? bar-state
                        @bar-derivation))


        (is (= #{2} @bar-derivation))
        (swap! foo-atom update :b inc)
        (is (= #{3} @bar-derivation))
        (is (not (identical? bar-state
                             @bar-derivation)))))))

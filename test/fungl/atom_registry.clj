(ns test.fungl.atom-registry
  (:require [fungl.application :as application]
            (fungl [cache :as cache]
                   [atom-registry :as atom-registry]
                   [value-registry :as value-registry]))
  (:use [clojure.test]))

(deftest cache-test
  (with-bindings (value-registry/state-bindings)
    (is (= 1 @(atom-registry/get! :baz {:create (fn [] 1)}))))
  
  
  (let [call-counts (atom {:foo 0
                           :bar 0
                           :baz 0})
        deleted-values (atom {})
        value-specification (fn [id]
                              {:create (fn [] 1)
                               :delete (fn [value-atom]
                                         (swap! deleted-values assoc id @value-atom))}) 
        baz (fn [bar-atom]
              (swap! call-counts update :baz inc)
              {:bar-atom-in-baz (atom-registry/deref! bar-atom)})
        bar (fn [x]
              (let [bar-atom (atom-registry/get! :bar (value-specification :bar))]
                (swap! call-counts update :bar inc)
                (conj {:bar-atom-in-bar @bar-atom
                       :argument-for-bar x}
                      (cache/call! baz bar-atom))))
        foo (fn [x]
              (let [foo-atom (atom-registry/get! :foo (value-specification :foo))]
                (swap! call-counts update :foo inc)
                (conj {:foo-atom-in-foo @foo-atom
                       :argument-for-foo x}
                      (cache/call! bar x))))]

    (println "--------------------")
    
    (with-bindings (conj (value-registry/state-bindings)
                         (cache/state-bindings))
      (testing "first call should result to evaluating all functions"
        (is (= {:foo-atom-in-foo 1,
                :argument-for-foo 10,
                :bar-atom-in-bar 1,
                :argument-for-bar 10,
                :bar-atom-in-baz 1}
               (cache/call! foo 10)))
        (is (= {:foo 1, :bar 1, :baz 1}
               @call-counts)))
      
      (testing "second call with the same argument should return result from the cache"
        (is (= {:foo-atom-in-foo 1,
                :argument-for-foo 10,
                :bar-atom-in-bar 1,
                :argument-for-bar 10,
                :bar-atom-in-baz 1}
               (cache/call! foo 10)))
        (is (= {:foo 1, :bar 1, :baz 1}
               @call-counts)))
      
      (testing "call with different argument should result to evaluating affected functions"
        (is (= {:foo-atom-in-foo 1,
                :argument-for-foo 5,
                :bar-atom-in-bar 1,
                :argument-for-bar 5,
                :bar-atom-in-baz 1}
               (cache/call! foo 5)))
        (is (= {:foo 2, :bar 2, :baz 1}
               @call-counts)))

      (testing "resetting a referenced atom should invalidate the affected cached values"
        (reset! (atom-registry/get! :bar (value-specification :bar))
                20)
        (is (= {:foo-atom-in-foo 1,
                :argument-for-foo 5,
                :bar-atom-in-bar 20,
                :argument-for-bar 5,
                :bar-atom-in-baz 20}
               (cache/call! foo 5)))
        (is (= {:foo 3, :bar 3, :baz 2}
               @call-counts)))

      (testing "resetting a referenced atom should not invalidate the unaffected cached values"
        (reset! (atom-registry/get! :foo (value-specification :foo))
                20)
        (is (= {:foo-atom-in-foo 20,
                :argument-for-foo 5,
                :bar-atom-in-bar 20,
                :argument-for-bar 5,
                :bar-atom-in-baz 20}
               (cache/call! foo 5)))
        (is (= {:foo 4, :bar 3, :baz 2}
               @call-counts)))
      
      (testing "deleting unused values should not delete values that were referenced after the last deletion"
        (value-registry/delete-unused-values! -1)
        (is (= {:foo-atom-in-foo 20,
                :argument-for-foo 5,
                :bar-atom-in-bar 20,
                :argument-for-bar 5,
                :bar-atom-in-baz 20}
               (cache/call! foo 5)))
        (is (= {:foo 4, :bar 3, :baz 2}
               @call-counts)))

      (testing "deleting unused values should delete values that were not referenced after the last deletion"
        (value-registry/delete-unused-values! -1)
        (is (= {:bar-atom-in-bar 20
                :argument-for-bar 5
                :bar-atom-in-baz 20}
               (cache/call! bar 5)))
        (is (= {:foo 4, :bar 3, :baz 2}
               @call-counts))
        
        (value-registry/delete-unused-values! -1)
        
        (is (= {:foo-atom-in-foo 1,
                :argument-for-foo 5,
                :bar-atom-in-bar 20,
                :argument-for-bar 5,
                :bar-atom-in-baz 20}
               (cache/call! foo 5)))
        (is (= {:foo 5, :bar 3, :baz 2}
               @call-counts)))

      (testing "deleting a value should call the associated destructor"
        (is (= {:foo 20}
               @deleted-values)))

      (testing "deleting unused values twise in a row should delete all values"
        (value-registry/delete-unused-values! -1)
        (value-registry/delete-unused-values! -1)
        (is (= {:foo-atom-in-foo 1,
                :argument-for-foo 5,
                :bar-atom-in-bar 1,
                :argument-for-bar 5,
                :bar-atom-in-baz 1}
               (cache/call! foo 5)))
        (is (= {:foo 6, :bar 4, :baz 3}
               @call-counts))))))




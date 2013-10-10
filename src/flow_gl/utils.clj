(ns flow-gl.utils
  (:use clojure.test))

(defn get-and-reset [atom key new-value]
  (let [now-key (keyword (namespace key) (str (name key) "-now"))]
    (swap! atom (fn [value]
                  (assoc value now-key (key value)
                         key new-value)))))

(deftest get-and-reset-test
  (is (= (get-and-reset (atom {::foo 4})
                        ::foo
                        0)
         {::foo-now 4, ::foo 0})))

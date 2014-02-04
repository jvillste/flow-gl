(ns flow-gl.utils
  (:use midje.sweet))

(defn map-vals [m f]
  (zipmap (keys m) (map f (vals m))))


(defn get-and-reset [atom key new-value]
  (let [now-key (keyword (namespace key) (str (name key) "-now"))]
    (swap! atom (fn [value]
                  (assoc value now-key (key value)
                         key new-value)))))

(fact get-and-reset-test
      (get-and-reset (atom {::foo 4})
                     ::foo
                     0)
      =>
      {::foo-now 4, ::foo 0})

(defmacro for-all [bindings body]
  `(doall (for ~bindings ~body)))

(defn map-all-indexed [function coll]
  (doall (map-indexed function coll)))



(defn in?
  "true if seq contains elm"
  [elm seq] 
  (some #(= elm %) seq))

(def ^:dynamic delayed-applications {})

(defn with-delayed-applications-function [key value function]
  (binding [delayed-applications (assoc delayed-applications key (atom []))]
    (let [value (function value)]
      (reduce (fn [value function]
                (function value))
              value
              @(key delayed-applications)))))

(defmacro with-delayed-applications [key value & body]
  `(with-delayed-applications-function ~key ~value (fn [~value] ~@body)))

(defn apply-later [key function]
  (if (contains? delayed-applications key)
    (swap! (key delayed-applications) conj function)
    (throw (Exception. (str "No delayed applications available for the key " key)))))


;; Tests


(fact delayed-applications-with-one-var
      (let [x {}]
        (with-delayed-applications :x x
          (assoc x :foo (do
                          (apply-later :x
                                       (fn [x] (assoc x :x-bar :x-bar-value)))

                          :foo-value))))

      => {:foo :foo-value
          :x-bar :x-bar-value})

(fact apply-later-for-wrong-key-throws-exception
      (let [x 1]
        (with-delayed-applications :x x
          (apply-later :y
                       (fn [x]))))
      => (throws Exception "No delayed applications available for the key :y"))

(fact delayed-applications-with-two-vars
        (let [x {}
              y {}]
          (with-delayed-applications :x x
            (assoc x :x-foo (with-delayed-applications :y y
                              (assoc y :y-foo (do
                                                (apply-later :x
                                                             (fn [x] (assoc x :x-bar :x-bar-value)))
                                                (apply-later :y
                                                             (fn [y] (assoc y :y-bar :y-bar-value)))
                                                :y-foo-value))))))
        => {:x-bar :x-bar-value
            :x-foo {:y-bar :y-bar-value
                    :y-foo :y-foo-value}})

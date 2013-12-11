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

(defmacro forall [bindings body]
  `(doall (for ~bindings ~body)))


(def ^:dynamic delayed-applications)

(defn with-delayed-applications-function [delayed-applications-var state function]
  (let [state (function state)]
    (reduce (fn [state function]
              (function state))
            state
            @delayed-applications-var)))

(defmacro with-delayed-applications [delayed-applications-var-symbol value & body]
  `(binding [~delayed-applications-var-symbol (atom [])]
     (with-delayed-applications-function ~delayed-applications-var-symbol ~value (fn [~value] ~@body))))

(defn apply-later [delayed-applications-var function]
  (if (bound? delayed-applications-var)
    (swap! (deref delayed-applications-var) conj function)
    (throw (Exception. (str delayed-applications-var " is not boundsdf")))))


;; Tests

(def ^:dynamic delayed-applications-to-x)
(def ^:dynamic delayed-applications-to-y)

(fact delayed-applications-with-one-var
      (let [x {}]
        (binding [delayed-applications-to-x :foo]

          (with-delayed-applications delayed-applications-to-x x
            (assoc x :foo (do
                            (apply-later #'delayed-applications-to-x
                                         (fn [x] (assoc x :x-bar :x-bar-value)))

                            :foo-value)))))

      => {:foo :foo-value
          :x-bar :x-bar-value})

#_(fact delayed-applications-with-two-vars
        (let [x {}
              y {}]
          (with-delayed-applications delayed-applications-to-x x
            (assoc x :x-foo (with-delayed-applications delayed-applications-to-y y
                              (assoc y :y-foo (do
                                                (apply-later delayed-applications-to-x
                                                             (fn [x] (assoc x :x-bar :x-bar-value)))
                                                (apply-later delayed-applications-to-y
                                                             (fn [y] (assoc y :y-bar :y-bar-value)))
                                                :y-foo-value))))))
        => {:x-bar :x-bar-value
            :x-foo {:y-bar :y-bar-value
                    :y-foo :y-foo-value}})

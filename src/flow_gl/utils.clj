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




(defn with-delayed-applications-function [delayed-applications-var state function]
  (let [state (function state)]
    (reduce (fn [state function]
              (function state))
            state
            @delayed-applications-var)))

(defmacro with-delayed-applications [delayed-applications-var value & body]
  `(binding [~delayed-applications-var (atom [])]
     (with-delayed-applications-function ~delayed-applications-var ~value (fn [~value] ~@body))))


(defn apply-delayed [delayed-applications-var function]
  (swap! delayed-applications-var conj function))



(def ^:dynamic test-var-x)

(fact (let [x 1]
        (binding [test-var-x (atom [])]
          (with-delayed-applications-function test-var-x x (fn [x] (+ 1 x))))))
(fact
    (let [x 1]
      (with-delayed-applications test-var-x x
        (apply-delayed test-var-x (fn [x] (+ x 1)))
        (+ x 1)))
    => 3)


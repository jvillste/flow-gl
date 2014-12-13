(ns flow-gl.gui.cache
  (:use clojure.test))

(def ^:dynamic cache)

(defn create []
  (atom {:used #{}}))

(defmacro with-cache [cache-to-be-used & body]
  `(binding [cache ~cache-to-be-used]
     ~@body))

(defn cached
  [f]
  (fn [& args]
    (if (bound? #'cache)
      (do (swap! cache update-in [:used] conj [f args])
          (if-let [value (get @cache [f args])]
            value
            (let [value (apply f args)]
              (swap! cache assoc [f args] value)
              value)))
      (apply f args))))

(defmacro defn-cached [name args & body]
  `(def ~name (cached (fn ~args ~@body))))

(defn clear-usages []
  (swap! cache assoc :used #{}))

(defn remove-unused []
  (swap! cache (fn [cache]
                 (reduce dissoc
                         cache
                         (->> (keys cache)
                              (filter (fn [key]
                                        (and (not (= key :used))
                                             (not (contains? (:used cache)
                                                             key))))))))))

;; Testing

(comment
  (defn-cached cached-function [data x]
    (+ (count data) x))

  (defmacro run-time [& body]
    `(do
       (println '~body)
       (dorun (repeatedly 10 (fn [] (time ~@body))))))

  (with-cache (create)
    (let [long-data (repeat 1000 1)
          long-data-2 (repeat 1000 1)
          short-data [1]]

      (run-time (cached-function long-data 1))
      (run-time (cached-function long-data-2 1))
      (run-time (cached-function short-data 1))))

  #_(with-cache (create)
      (cached-function [1] 1)
      (cached-function [1] 2)
      (clear-usages)
      (println (keys @cache))
      (cached-function [1] 1)
      (remove-unused)
      (println (keys @cache))))

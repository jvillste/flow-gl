(ns flow-gl.gui.cache
  (:require [flow-gl.thread-inheritable :as thread-inheritable])
  (:use clojure.test))

(def cache-enabled? (thread-inheritable/thread-inheritable true))

(defmacro with-cache-disabled
  [& body]
  `(thread-inheritable/inheritable-binding [cache-enabled? false]
                                           ~@body))

(def ^:dynamic cache)

(defn create []
  (atom {:used #{}}))

(defmacro with-cache [cache-to-be-used & body]
  `(binding [cache ~cache-to-be-used]
     ~@body))

(defn cached [f]
  (fn [& args]
    (if (and true #_@cache-enabled?
             (bound? #'cache))
      (do (swap! cache update-in [:used] conj [f args])
          (if-let [value (get @cache [f args])]
            (do (flow-gl.debug/add-event [:cache-hit (:name (meta f))] )
                #_(if (= (:type (meta f)) :layout)
                    (flow-gl.gui.transformer/with-transformers
                      (flow-gl.gui.transformer/->HighlightAll :highlight [0 255 0 100])
                      value)
                    value)
                value)
            (let [value (apply f args)]

              #_(println "missed " (:name (meta f)) "but found" (->> (keys @cache)
                                                                     (filter vector?)
                                                                     (filter (fn [[f2 args2]] (= f2 f)))
                                                                     (map (fn [[f2 args2]] (take 2 (clojure.data/diff args args2))))
                                                                     #_(map keys)))
              #_(when (= (:name (meta f))
                         "counter-view")
                  (println "missed " (:name (meta f)) "but found" (->> (keys @cache)
                                                                       (filter vector?)
                                                                       (filter (fn [[f2 args2]] (= f2 f)))
                                                                       (map (fn [[f2 args2]] (take 2 (clojure.data/diff args args2)))))))

              (swap! cache assoc [f args] value)
              (flow-gl.debug/add-event [:cache-miss (:name (meta f))])
              #_(if (= (:type (meta f)) :layout)
                  (flow-gl.gui.transformer/with-transformers
                    (flow-gl.gui.transformer/->HighlightAll :highlight [255 0 0 10])
                    value)
                  value)
              value)))
      (apply f args))))

(defmacro defn-cached [name args & body]
  `(def ~name (cached (with-meta (fn ~args ~@body)
                        {:name ~(str name)}))))

(defn call-with-cache [cache function & arguments]
  (let [cache (update-in cache [:used] conj [function arguments])]
    (if-let [value (get cache [function arguments])]
      [cache value]
      (let [value (apply function arguments)
            cache (assoc cache [function arguments] value)]
        [cache value]))))

(defn clear-usages [cache]
  (assoc cache :used #{}))

(defn remove-unused [cache]
  #_(println "unused"
             (count (->> (keys @cache)
                         (filter (fn [key]
                                   (and (not (= key :used))
                                        (not (contains? (:used @cache)
                                                        key)))))))
             (count (keys @cache)))

  (reduce dissoc
          cache
          (->> (keys cache)
               (filter (fn [key]
                         (and (not (= key :used))
                              (not (contains? (:used cache)
                                              key))))))))

;; Testing

(comment
  (defn-cached cached-function [data x]
    (println "run")
    (+ (count data) x))

  (with-cache-disabled
    (.start (Thread. (fn [] (with-cache (create)
                              (let [short-data [1]]

                                (cached-function short-data 1)
                                (cached-function short-data 1)))))))



  (defn-cached cached-function [data x]
    (+ (count data) x))

  (defmacro run-time [& body]
    `(do
       (println ~@body)
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

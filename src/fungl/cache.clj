(ns fungl.cache
  (:import [com.google.common.cache CacheBuilder CacheLoader]
           [java.util.concurrent TimeUnit])
  (:use clojure.test))


(defn create [& args]
  (-> (CacheBuilder/newBuilder)
      #_(.expireAfterAccess 30 TimeUnit/SECONDS)
      (.maximumSize 100)
      (.build (proxy [CacheLoader] []
                (load [[function arguments]] (apply function arguments))))))


(defn call-with-cache [cache function & arguments]
  (.get cache [function arguments]))


(def ^:dynamic cache)

(defn state-bindings []
  {#'cache (create)})

(defn call [function & arguments]
  (apply call-with-cache
         cache
         function
         arguments))



(defn start []
  (let [f (fn [x]
            (println "called with " x)
            x)]
    (with-bindings (state-bindings)
      (println "got" (call-cached f 1))
      (println "got" (call-cached f 2))
      (println "got" (call-cached f 2))
      (println "got" (call-cached f 1)))))

(ns fungl.cache
  (:require [fungl.depend :as depend])
  (:import [com.google.common.cache CacheBuilder CacheLoader RemovalListener]
           [java.util.concurrent TimeUnit])
  (:use clojure.test))

(def ^:dynamic state)


(defn create-state [& args]
  {:cache (-> (CacheBuilder/newBuilder)
              #_(.expireAfterAccess 30 TimeUnit/SECONDS)
              (.maximumSize 100)
              (.build (proxy [CacheLoader] []
                        (load [[function arguments]]
                          #_(println "calling " function)
                          (depend/with-dependencies
                            (let [result (apply function arguments)]
                              (swap! (:dependencies state)
                                     assoc
                                     [function arguments]
                                     (depend/current-dependencies))
                              result)))))
              
              #_(.removalListener (proxy [RemovalListener] []
                                    (onRemoval [removal-notification]
                                      (prn "removed from cache:" (.getKey removal-notification))))))
   :dependencies (atom {})})

(defn call-with-cache [state function & arguments]
  (loop [dependencies (get @(:dependencies state)
                           [function arguments])]
    (if-let [[dependency value] (first dependencies)]
      (if (not= value (depend/current-value dependency))
        (do #_(println "invalidating " [function arguments]
                     "because" value " is not "(depend/current-value dependency))
            (swap! (:dependencies state)
                   dissoc [function arguments])
            (.invalidate (:cache state) [function arguments]))
        (recur (rest dependencies)))
      (doseq [[dependency value] (get @(:dependencies state)
                                      [function arguments])]
        #_(println "adding dependency to " (:id dependency)
                   (:type dependency))
        (depend/add-dependency dependency value))))
  
  (.get (:cache state) [function arguments]))


(defn state-bindings []
  {#'state (create-state)})

(defn call! [function & arguments]
  (apply call-with-cache
         state
         function
         arguments))


(defn start []
  (let [f (fn [x]
            (println "called with " x)
            x)]
    (with-bindings (state-bindings)
      (println "got" (call! f 1))
      (println "got" (call! f 2))
      (println "got" (call! f 2))
      (println "got" (call! f 1)))))

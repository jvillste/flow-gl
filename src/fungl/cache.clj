(ns fungl.cache
  (:require [fungl.depend :as depend]
            #_[flow-gl.tools.trace :as trace])
  (:import [com.google.common.cache CacheBuilder CacheLoader RemovalListener]
           [java.util.concurrent TimeUnit])
  (:use clojure.test))

(def ^:dynamic state)


(defn create-state [& args]
  {:cache (-> (CacheBuilder/newBuilder)
              #_(.expireAfterAccess 30 TimeUnit/SECONDS)
              (.maximumSize 100000)
              (.build (proxy [CacheLoader] []
                        (load [[function arguments]]
                          #_(trace/log "loading to cache" function arguments)
                          (depend/with-dependencies
                            (let [result (apply function arguments)]
                              (swap! (:dependencies state)
                                     assoc
                                     [function arguments]
                                     (depend/current-dependencies))
                              result))))))
   :dependencies (atom {})})

(defn call-with-cache [state function & arguments]

  #_(apply function arguments)

  #_(flow-gl.tools.trace/log "dependencies" (get @(:dependencies state)
                                               [function arguments]))
  (loop [dependencies (get @(:dependencies state)
                           [function arguments])]
    (if-let [[dependency value] (first dependencies)]
      (if (not= value (depend/current-value dependency))
        (do #_(println "invalidating " [function arguments]
                       "because" value " is not "(depend/current-value dependency))
            #_(flow-gl.tools.trace/log "invalidating" function arguments
                                     "because not" value "=" (depend/current-value dependency))
            (swap! (:dependencies state)
                   dissoc [function arguments])
            (.invalidate (:cache state) [function arguments]))
        (recur (rest dependencies)))
      
      (doseq [[dependency value] (get @(:dependencies state)
                                      [function arguments])]
        #_(println "adding dependency to " (:id dependency)
                   (:type dependency))
        (depend/add-dependency dependency value))))

  (when (.containsKey (.asMap (:cache state))
                      [function arguments])
    #_(trace/log "cache hit" function arguments))
  
  
  (.get (:cache state) [function arguments]))




(defn state-bindings []
  {#'state (create-state)})

(defn call! [function & arguments]
  (if (bound? #'state)
    (apply call-with-cache
           state
           function
           arguments)
    (apply function
           arguments)))

(defmacro defn-memoized [name arguments & body]
  
  (let [implementation-name (symbol (namespace name)
                                    (str name "-implementation"))]
    `(do (declare ~name)
         (defn ~implementation-name ~arguments
           ~@body)
         (defn ~name ~arguments
           (call! ~implementation-name ~@arguments)))))

(defmacro def-10 [name]
  `(def ~name 10))

(defmacro def-add-10 [name name2]
  `(def ~name (+ ~name2 10)))



(defn start []
  (let [f (fn [x]
            (println "called with " x)
            x)]
    (with-bindings (state-bindings)
      (println "got" (call! f 1))
      (println "got" (call! f 2))
      (println "got" (call! f 2))
      (println "got" (call! f 1)))))
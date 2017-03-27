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

(defn handle-dependencies [function arguments]
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
        (depend/add-dependency dependency value)))))

(defn call-with-cache [state function & arguments]

  #_(flow-gl.tools.trace/log "dependencies" (get @(:dependencies state)
                                                 [function arguments]))
  (handle-dependencies function arguments)

  #_(when (.containsKey (.asMap (:cache state))
                        [function arguments])
      #_(trace/log "cache hit" function arguments))
  
  (.get (:cache state) [function arguments]))


;; TODO: Anonymous funciton can not be a cache key because it is a different function on every call
;; Can guava cache hold value for wich the key does not contain all the information that is needed to compute the value?
(defn call-with-cache-and-key [state cache-key function & arguments]

  (handle-dependencies function arguments)
  
  (.get (:cache state) [function cache-key]
        (fn [] (apply function arguments))))

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

(defn call-with-key! [function cache-key & arguments]
  (if (bound? #'state)
    (apply call-with-cache-and-key 
           state
           cache-key
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


(deftest cache-test
  (let [call-count (atom 0)
        f (fn [x]
            (swap! call-count inc)
            {:call-count @call-count
             :result x})]
    
    (with-bindings (state-bindings)

      (is (= 1
             (.get (:cache state) :x
                   (fn [] 1))))
      
      (is (= {:call-count 1, :result 1}
             (call! f 1)))

      (is (= {:call-count 1, :result 1}
             (call! f 1)))
      
      (is (= {:call-count 2, :result 2}
             (call! f 2)))

      (is (= {:call-count 3, :result 2}
             (call-with-key! f :x 2)))

      (is (= {:call-count 3, :result 2}
             (call-with-key! f :x 3)))

      (is (= {:call-count 4, :result 3}
             (call-with-key! f :y 3))))))


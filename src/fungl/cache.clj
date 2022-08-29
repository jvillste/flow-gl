(ns fungl.cache
  (:require [clojure.test :refer :all]
            [fungl.depend :as depend])
  (:import (com.google.common.cache CacheBuilder CacheLoader)))

(def ^:dynamic state)

(defn stats [state]
  (let [stats (.stats (:cache state))]
    {:size (.size (:cache state))
     :hit-count (.hitCount stats)
     :request-count (.requestCount stats)
     :load-success-count (.loadSuccessCount stats)}))

(defn create-state [maximum-size]
  (let [dependencies-atom (atom {})]
    {:cache (-> (CacheBuilder/newBuilder)
                #_(.expireAfterAccess 30 TimeUnit/SECONDS)
                (.maximumSize maximum-size)
                (.build (proxy [CacheLoader] []
                          (load [[function arguments]]
                            (depend/with-dependencies
                              (let [result (apply function arguments)]
                                (swap! dependencies-atom
                                       assoc
                                       [function arguments]
                                       (depend/current-dependencies))
                                result))))))
     :dependencies dependencies-atom}))

(comment
  (let [state (create-state 10)]
    (prn (stats state))
    (prn (.get (:cache state) [+ [1 2]]))
    (prn (stats state))
    (prn (.get (:cache state) [+ [1 2]]))
    (prn (stats state)))

  ) ;; TODO: remove-me

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

(defn in-use? []
  (bound? #'state))

(defn function-call-key [function arguments]
  [function arguments])

(defn cached? [function & arguments]
  (and (in-use?)
       (boolean (.getIfPresent (:cache state) (function-call-key function arguments)))))

(defn get [key]
  (when (in-use?)
    (.getIfPresent (:cache state) key)))



(defn put! [key result]
  (when (in-use?)
    (.put (:cache state)
          key
          result)))

(defn invalidate! [key]
  (when (in-use?)
    (.invalidate (:cache state)
                 key)))

(defn invalidate-all! []
  (when (in-use?)
    (.invalidateAll (:cache state))))

;; TODO: Anonymous funciton can not be a cache key because it is a different function on every call
;; Can guava cache hold value for wich the key does not contain all the information that is needed to compute the value?
(defn call-with-cache-and-key [state cache-key function & arguments]

  (handle-dependencies function arguments)

  (.get (:cache state) [function cache-key]
        (fn [] (apply function arguments))))

(defn state-bindings []
  {#'state (create-state 100)})

(defn cached []
  (.keySet (.asMap (:cache state))))

(defn call! [function & arguments]
  (if (in-use?)
    (apply call-with-cache
           state
           function
           arguments)
    (apply function
           arguments)))

(defn call-with-key! [function cache-key & arguments]
  (if (in-use?)
    (apply call-with-cache-and-key
           state
           cache-key
           function
           arguments)
    (apply function
           arguments)))

(defn memoized [function]
  (fn [& arguments]
    (apply call! function arguments)))

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

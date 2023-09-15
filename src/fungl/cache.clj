(ns fungl.cache
  (:require [clojure.test :refer :all]
            [fungl.depend :as depend]
            [medley.core :as medley]
            [clojure.string :as string])
  (:import (com.google.common.cache CacheBuilder CacheLoader)))

(def ^:dynamic state)

(defn size
  ([]
   (size state))
  ([state]
   (.size ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state))))

(defn stats [state]
  (let [stats (.stats (:cache state))]
    {:size (.size (:cache state))
     :hit-count (.hitCount stats)
     :request-count (.requestCount stats)
     :load-success-count (.loadSuccessCount stats)}))

(defn call-and-return-result-and-dependencies [function arguments]
  (depend/with-dependencies
    (let [result (apply function arguments)]
      {:result result
       :dependencies (depend/current-dependencies)})))

(defn set-dependencies! [function arguments dependencies]
  (when (not (empty? dependencies))
    (swap! (:dependencies state)
           assoc
           [function (or arguments
                         [])]
           dependencies)))

(defn all-dependables []
  (->> @(:dependencies state)
       (vals)
       (mapcat keys)
       (distinct)))

(defn as-map []
  (.asMap ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state)))

(defn create-state [maximum-size]
  (let [dependencies-atom (atom {})]
    {:cache (-> (CacheBuilder/newBuilder)
                #_(.expireAfterAccess 30 TimeUnit/SECONDS)
                (.maximumSize maximum-size)
                (.build (proxy [CacheLoader] []
                          (load [[function arguments]]
                            (let [{:keys [result dependencies]} (call-and-return-result-and-dependencies function arguments)]
                              (set-dependencies! function
                                                 arguments
                                                 dependencies)
                              (if (nil? result)
                                ::nil
                                result))))))
     :dependencies dependencies-atom}))

(comment
  (with-bindings (state-bindings)
    (prn (.asMap (:cache state)))
    (prn (.get (:cache state) [+ [1 2]]))
    (prn (.asMap (:cache state))))

  (with-bindings (state-bindings)
    (prn (stats state))
    (prn (.get (:cache state) [+ [1 2]]))
    (prn (stats state))
    (prn (.get (:cache state) [+ [1 2]]))
    (prn (stats state))
    (prn (.get (:cache state) [+ [1 3]]))
    (prn (stats state)))) ;; TODO: remove-me


(defn function-call-key [function arguments]
  [function (or arguments
                [])])

(defn dependency-value-map [function arguments]
  (clojure.core/get @(:dependencies state)
                    (function-call-key function arguments)))

(defn changed-dependency-value? [dependency-value-pair]
  (let [[dependency value] dependency-value-pair]
    (not= value (depend/current-value dependency))))

(defn should-be-invalidated? [function arguments]
  (some changed-dependency-value?
        (dependency-value-map function arguments)))



(defn invalidate-cache-if-needed [function arguments]
  (when (should-be-invalidated? function arguments)
    (swap! (:dependencies state)
           dissoc (function-call-key function arguments))
    (.invalidate ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state) (function-call-key function arguments))))



(defn call-with-cache [state function & arguments]
  (invalidate-cache-if-needed function arguments)
  ;; (def state state) ;; TODO: remove me
  ;; (def function function) ;; TODO: remove me
  ;; (def arguments arguments) ;; TODO: remove me
  (let [result (.get ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state)
                     (function-call-key function
                                        arguments))]
    (if (= ::nil result)
      nil
      result)))

(defn in-use? []
  (bound? #'state))



(defn cached? [function & arguments]
  (and (in-use?)
       (boolean (.getIfPresent ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state)
                               (function-call-key function arguments)))))

(defn cached-2? [state function & arguments]
  (and (in-use?)
       (boolean (.getIfPresent ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state)
                               (function-call-key function arguments)))))

(defn cache-is-valid? [function arguments]
  (and (apply cached? function arguments)
       (not (should-be-invalidated? function arguments))))

(defn get [key]
  (when (in-use?)
    (.getIfPresent ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state)
                   key)))



(defn put! [key result]
  (when (in-use?)
    (.put ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state)
          key
          result)))

(defn invalidate! [key]
  (when (in-use?)
    (.invalidate ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state)
                 key)))

(defn invalidate-all! []
  (when (in-use?)
    (.invalidateAll ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state))))

;; TODO: Anonymous funciton can not be a cache key because it is a different function on every call
;; Can guava cache hold value for wich the key does not contain all the information that is needed to compute the value?
(defn call-with-cache-and-key [state cache-key function & arguments]

  (invalidate-cache-if-needed function arguments)

  (.get ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state) [function cache-key]
        (fn [] (apply function arguments))))

(defn state-bindings []
  {#'state (create-state 5000)})

(defn cached []
  (.keySet (.asMap ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state))))

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
           (call! ~implementation-name ~@arguments))
         (alter-meta! (var ~implementation-name)
                      assoc
                      :private true
                      ::cached (var ~name))
         (alter-meta! (var ~name)
                      assoc
                      ::uncached (var ~implementation-name)))))

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
             (.get ^com.google.common.cache.LocalCache$LocalLoadingCache (:cache state) :x
                   (fn [] 1))))

      (is (not (cached? f 1)))
      (is (not (cached-2? state f 1)))

      (is (= {:call-count 1, :result 1}
             (call! f 1)))

      (is (cached? f 1))
      (is (cached-2? state f 1))
      (is (not (cached? f 2)))
      (is (not (cached? (fn []) 1)))

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


(comment
  (with-bindings (state-bindings)
    (let [f (fn [])]
      (call! f)))
  ) ;; TODO: remove-me

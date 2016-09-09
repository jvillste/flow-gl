(ns flow-gl.profiling
  (:require [taoensso.timbre.profiling :as profiling]))

(defmacro p [& args]
  `(profiling/p ~@args))

(defmacro pdefn [funciton-name arguments & body]
  `(defn ~funciton-name ~arguments
     (profiling/p ~(keyword (name funciton-name ))
                  ~@body)))



(defn profile-fn-call [funciton-name f arguments]
  (profiling/p (keyword (name funciton-name ))
               (apply f arguments)))


(defn untrace-var
  ([ns s]
   (untrace-var (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)
         f  ((meta v) ::profiled)]
     (when f
       (doto v
         (alter-var-root (constantly ((meta v) ::profiled)))
         (alter-meta! dissoc ::profiled))))))

(defn untrace-ns [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
      (untrace-var f))))

#_(defmacro untrace-ns [ns]
    `(untrace-ns* ~ns))


(defn profile-var
  ([ns s]
   (profile-var (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)]
     (if (and (ifn? @v)
              (-> v meta :macro not)
              (-> v meta ::profiled not))
       (do (println "profiling" s)
           (let [f @v
                 vname (symbol (str ns "/" s))]
             (doto v
               (alter-var-root #(fn profiling-wrapper [& args]
                                  (profile-fn-call vname % args)))
               (alter-meta! assoc ::profiled f))))))))

(defn namespace-function-vars [namespace]
  (->> namespace ns-interns vals (filter (fn [v] (and (-> v var-get fn?)
                                                      (not (-> v meta :macro)))))))

(defn profile-ns [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core flow-gl.profiling} (.name ns))
      (let [ns-fns (namespace-function-vars ns)]
        (doseq [f ns-fns]
          (profile-var f))))))

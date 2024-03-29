(ns flow-gl.profiling  (:require [taoensso.tufte :as profiling]))

(defmacro p [& args]
  `(profiling/p ~@args))

(defmacro pdefn [function-name arguments & body]
  `(defn ~function-name ~arguments
     (profiling/p ~(keyword (name function-name ))
                  ~@body)))


(defn profile-fn-call [function-name f arguments]
  (profiling/p (keyword (namespace function-name)
                        (name function-name))
               (apply f arguments)))


(defn uprofile-var
  ([ns s]
   (uprofile-var (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)
         f  ((meta v) ::profiled)]
     (when f
       (doto v
         (alter-var-root (constantly ((meta v) ::profiled)))
         (alter-meta! dissoc ::profiled))))))

(defn unprofile-ns [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
      (uprofile-var f))))

#_(defmacro unprofile-ns [ns]
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

(defmacro with-profiling [& args]
  `(profiling/profile ~@args))

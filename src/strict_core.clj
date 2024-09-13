(ns strict-core
  (:refer-clojure :exclude [get-in]))

(defn get-in
  ([m ks]
   (assert (instance? clojure.lang.Associative
                      m))
   (assert (instance? clojure.lang.Seqable
                      ks))
   (get-in m ks))

  ([m ks not-found]
   (assert (instance? clojure.lang.Associative
                      m))
   (assert (instance? clojure.lang.Seqable
                      ks))
   (get-in m ks not-found)))
